{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Main
Description : OpenCL Hello World Example
Copyright   : (c) Jonathan Merritt, 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : experimental
Portability : POSIX + OpenCL

This is a Hello World OpenCL example that stores buffer data in vectors.
-}

module Main where

import           Control.Parallel.CLUtil         (OpenCLState (OpenCLState),
                                                  bufferToVector, clContext,
                                                  clDevice, clQueue,
                                                  writeVectorToBuffer)
import           Control.Parallel.OpenCL

import           Control.Monad                   (forM_)
import           Data.Vector.Storable            (Vector)
import qualified Data.Vector.Storable            as V
import           Foreign                         (nullPtr, sizeOf)
import           Foreign.C.Types                 (CFloat)
import           Language.C.Quote.OpenCL         (cfun)
import           Text.PrettyPrint.Mainland       (prettyCompact)
import           Text.PrettyPrint.Mainland.Class (ppr)



-- | The kernel to execute: the equivalient of 'map (*2)'.
kernelSource :: String
kernelSource = prettyCompact . ppr $ [cfun|
    /* This example kernel just does `map (*2)` */
    kernel void doubleArray(
        global float *in,
        global float *out
    ) {
        int i = get_global_id(0);
        out[i] = 2 * in[i];
    }
|]



main :: IO ()
main = do
    putStrLn "* Hello World OpenCL Example *"

    -- Describe the OpenCL Environment
    putStrLn "\n* OpenCL Platform Environment *"
    describePlatforms

    -- Create a Context, Queue and a CLUtil OpenCLState
    context <- clCreateContextFromType [] [CL_DEVICE_TYPE_CPU] print
    device  <- head <$> clGetContextDevices context
    queue   <- clCreateCommandQueue context device []
    -- NB: OpenCLState is used by CLUtil when manipulating vector buffers
    let state = OpenCLState
                { clDevice  = device
                , clContext = context
                , clQueue   = queue
                }

    -- Create the Kernel
    program <- clCreateProgramWithSource context kernelSource
    clBuildProgram program [device] ""
    kernel <- clCreateKernel program "doubleArray"

    -- Set up memory
    let
        inputData :: Vector CFloat
        inputData = V.fromList [(-4) .. 4]

        nElem  = V.length inputData
        nBytes = nElem * sizeOf (undefined :: CFloat)

    -- Buffers for input and output data.
    -- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
    -- since we're using CPU. The performance here may not be ideal, because
    -- we're copying the buffer. However, it's safe, and not unduly nested.
    bufIn <- clCreateBuffer context
                            [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR]
                            (nBytes, nullPtr)
    bufOut <- clCreateBuffer context
                             [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR]
                             (nBytes, nullPtr)

    -- Copy our input data Vector to the input buffer; blocks until complete
    writeVectorToBuffer state bufIn inputData

    -- Run the kernel
    clSetKernelArgSto kernel 0 bufIn
    clSetKernelArgSto kernel 1 bufOut
    execEvent <- clEnqueueNDRangeKernel queue kernel [nElem] [] []

    -- Get the result; blocks until complete
    outputData <- bufferToVector queue
                                 bufOut
                                 nElem
                                 [execEvent]
                                 :: IO (Vector CFloat)

    -- Clean up the Context
    _ <- clReleaseContext context

    -- Show our work
    putStrLn "\n* Results *"
    putStrLn $ "Input:  " ++ show inputData
    putStrLn $ "Output: " ++ show outputData



-- | Summarises the OpenCL Platforms and their Devices.
--
--   The names of Platforms and all Devices belonging to them are printed to
--   stdout.
describePlatforms :: IO ()
describePlatforms = do

    -- fetch the list of OpenCL Platforms
    platformList <- clGetPlatformIDs :: IO [CLPlatformID]

    -- for each platform,
    forM_ platformList $ \platform -> do

        -- fetch the list of OpenCL Devices
        devs <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL :: IO [CLDeviceID]

        -- print the Platform name and Device names
        pname platform
        forM_ devs dname

  where
      putPair name value = putStrLn (name ++ value)
      pname p = clGetPlatformInfo p CL_PLATFORM_NAME >>= putPair "Platform: "
      dname d = clGetDeviceName d                    >>= putPair "  Device: "

