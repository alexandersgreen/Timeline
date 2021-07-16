{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Prelude hiding (fail,concat)

import Control.Monad.Fail
import Data.Char
import Data.Semigroup ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Options.Applicative
import System.Environment
import Text.Printf
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Text (Text, unpack)
import Data.ByteString.Lazy (ByteString,concat)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import GHC.Generics (Generic)
import System.Time.Extra
import Data.Maybe
import Data.List.Extra

import Debug.Trace (trace)

-- used by the open street map API 
userEmail :: String
userEmail = error "Please add your email address in order to use the open street map API"

{-
<when>2018-03-16T12:49:03Z</when>
<gx:coord>-0.3105322 51.7439135 140</gx:coord>
...
-}

-- Timestamps that correspond to spurious locations
spurious :: Set.Set String
spurious = Set.fromList 
    [ "<when>2017-10-25T07:26:12Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:22:19Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:17:51Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:17:26Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:17:07Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:14:53Z</when>" -- <gx:coord>77.62531899999999 12.9990014 0</gx:coord>
    , "<when>2017-10-25T07:14:35Z</when>" -- <gx:coord>77.6253203 12.998993 0</gx:coord>  
    , "<when>2017-07-13T20:22:34Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:20:21Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:20:00Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:19:44Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:14:17Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:13:47Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:13:27Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>  
    , "<when>2019-08-18T14:26:35Z</when>" -- <gx:coord>12.2465239 52.1300721 1970</gx:coord>
    , "<when>2019-08-18T14:26:51Z</when>" -- <gx:coord>12.8436627 52.251509299999995 86</gx:coord>
    , "<when>2019-09-19T19:39:18Z</when>" -- <gx:coord>-1.7832800999999998 53.650423599999996 0</gx:coord>
    , "<when>2019-09-19T20:20:56Z</when>" -- <gx:coord>-1.7832800999999998 53.650423599999996 0</gx:coord>
    , "<when>2019-09-19T20:22:58Z</when>" -- <gx:coord>-1.7832800999999998 53.650423599999996 0</gx:coord>    
    , "<when>2010-04-03T07:57:57Z</when>" -- <gx:coord>-2.712908 51.386505 0</gx:coord>
    , "<when>2010-12-08T20:28:16Z</when>" -- <gx:coord>-1.125262 52.63218200000001 0</gx:coord>
    , "<when>2011-01-27T19:55:46Z</when>" -- <gx:coord>-0.743085 51.524133 0</gx:coord>
    , "<when>2012-03-14T00:44:48Z</when>" -- <gx:coord>-87.615195 41.8531572 0</gx:coord>
    , "<when>2013-10-09T14:48:29Z</when>" -- <gx:coord>-1.6595726000000002 52.5813169 0</gx:coord>
    , "<when>2013-10-09T14:49:29Z</when>" -- <gx:coord>-1.6595726000000002 52.5813169 0</gx:coord>
    , "<when>2013-10-09T14:50:29Z</when>" -- <gx:coord>-1.6595726000000002 52.5813169 0</gx:coord>
    , "<when>2013-10-16T07:57:10Z</when>" -- <gx:coord>-3.10574 51.012256099999995 0</gx:coord>
    , "<when>2013-10-16T07:58:05Z</when>" -- <gx:coord>-3.10574 51.012256099999995 0</gx:coord>
    , "<when>2013-10-16T08:59:38Z</when>" -- <gx:coord>-2.4247771 51.5416567 0</gx:coord>
    , "<when>2013-11-04T16:24:19Z</when>" -- <gx:coord>-0.2504976 51.7045421 0</gx:coord>
    , "<when>2013-11-04T16:25:19Z</when>" -- <gx:coord>-0.2504976 51.7045421 0</gx:coord>
    , "<when>2013-11-04T16:26:20Z</when>" -- <gx:coord>-0.2504976 51.7045421 0</gx:coord>
    , "<when>2013-11-04T17:38:36Z</when>" -- <gx:coord>68.8674501 40.623295 0</gx:coord>
    , "<when>2013-11-04T17:38:38Z</when>" -- <gx:coord>68.8674501 40.623295 0</gx:coord>
    , "<when>2013-11-04T17:38:39Z</when>" -- <gx:coord>68.8674501 40.623295 0</gx:coord>
    , "<when>2013-12-14T07:07:46Z</when>" -- <gx:coord>19.7011352 39.7736497 0</gx:coord>
    , "<when>2013-12-14T07:08:46Z</when>" -- <gx:coord>19.7011352 39.7736497 0</gx:coord>
    , "<when>2013-12-14T07:09:46Z</when>" -- <gx:coord>19.7011352 39.7736497 0</gx:coord>
    , "<when>2013-12-14T07:10:46Z</when>" -- <gx:coord>19.7011352 39.7736497 0</gx:coord>
    , "<when>2013-12-14T07:34:24Z</when>" -- <gx:coord>-0.0934907 51.5151543 0</gx:coord>
    , "<when>2014-04-12T12:01:40Z</when>" -- <gx:coord>-120.16852679999998 34.6685759 0</gx:coord>
    , "<when>2015-05-17T14:28:33Z</when>" -- <gx:coord>55.2873341 25.226032000000004 0</gx:coord>
    , "<when>2015-05-17T16:10:58Z</when>" -- <gx:coord>55.287345699999996 25.226032999999997 0</gx:coord>
    , "<when>2015-09-22T16:30:07Z</when>" -- <gx:coord>-3.6905520999999997 40.4081062 0</gx:coord>
    , "<when>2015-09-25T16:33:10Z</when>" -- <gx:coord>-1.0848172999999999 53.9578207 0</gx:coord>
    , "<when>2015-09-25T16:33:17Z</when>" -- <gx:coord>-1.0848172999999999 53.9578207 0</gx:coord>
    , "<when>2015-10-13T16:29:01Z</when>" -- <gx:coord>-3.8155169 53.1419783 0</gx:coord>
    , "<when>2015-10-13T16:36:38Z</when>" -- <gx:coord>-3.8155140999999997 53.1419753 0</gx:coord>
    , "<when>2015-10-13T16:49:53Z</when>" -- <gx:coord>-3.8155172999999993 53.141976899999996 0</gx:coord>
    , "<when>2015-10-13T16:53:50Z</when>" -- <gx:coord>-0.4801826 51.9032876 0</gx:coord>
    , "<when>2015-11-12T22:07:40Z</when>" -- <gx:coord>5.3683868 44.755640199999995 0</gx:coord>
    , "<when>2015-11-12T22:12:37Z</when>" -- <gx:coord>5.3683868 44.755640199999995 0</gx:coord>
    , "<when>2015-12-23T18:08:33Z</when>" -- <gx:coord>-1.7340535 52.4566264 0</gx:coord>
    , "<when>2015-12-23T18:09:08Z</when>" -- <gx:coord>-1.7340535 52.4566264 0</gx:coord>
    , "<when>2015-12-23T18:09:24Z</when>" -- <gx:coord>-1.7340518 52.4566266 0</gx:coord>
    , "<when>2015-12-23T18:11:08Z</when>" -- <gx:coord>-1.7340518 52.4566266 0</gx:coord>
    , "<when>2016-01-21T14:00:56Z</when>" -- <gx:coord>-2.5554259999999998 51.5068615 0</gx:coord>
    , "<when>2016-01-21T14:01:15Z</when>" -- <gx:coord>-2.5554266 51.506861799999996 0</gx:coord>
    , "<when>2016-01-30T22:21:55Z</when>" -- <gx:coord>-1.4166261 50.908394799999996 0</gx:coord>
    , "<when>2016-06-20T16:41:24Z</when>" -- <gx:coord>-1.5174044 53.7737896 0</gx:coord>
    , "<when>2016-06-20T16:57:20Z</when>" -- <gx:coord>-1.5174081 53.7737884 0</gx:coord>
    , "<when>2016-07-19T07:27:04Z</when>" -- <gx:coord>12.4828507 41.8505644 0</gx:coord>
    , "<when>2016-07-26T16:31:35Z</when>" -- <gx:coord>8.3097508 47.053809699999995 0</gx:coord>
    , "<when>2016-08-12T06:48:10Z</when>" -- <gx:coord>-0.1600348 51.507288599999995 0</gx:coord>
    , "<when>2016-08-12T06:06:46Z</when>" -- <gx:coord>-3.1786727999999997 51.4791726 0</gx:coord>
    , "<when>2016-08-12T06:07:46Z</when>" -- <gx:coord>-3.1786727999999997 51.4791726 0</gx:coord>
    , "<when>2016-08-13T15:04:24Z</when>" -- <gx:coord>-0.1600346 51.507288499999994 0</gx:coord>
    , "<when>2016-08-13T17:27:26Z</when>" -- <gx:coord>-2.3668066999999997 51.3741804 0</gx:coord>
    , "<when>2016-08-13T17:29:42Z</when>" -- <gx:coord>-2.366806 51.3741804 0</gx:coord>
    , "<when>2016-08-13T17:31:43Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:33:52Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:35:52Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:37:52Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:39:53Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:41:54Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:44:03Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:46:04Z</when>" -- <gx:coord>-2.3668063 51.3741806 0</gx:coord>
    , "<when>2016-08-13T17:51:00Z</when>" -- <gx:coord>-2.3668058 51.3741828 0</gx:coord>
    , "<when>2016-08-13T17:51:20Z</when>" -- <gx:coord>-2.3668058 51.3741828 0</gx:coord>
    , "<when>2016-08-13T17:53:27Z</when>" -- <gx:coord>-2.3668058 51.3741828 0</gx:coord>
    , "<when>2016-08-13T17:55:28Z</when>" -- <gx:coord>-2.3668058 51.3741828 0</gx:coord>
    , "<when>2016-08-13T17:57:30Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T17:59:41Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T18:01:42Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T18:03:44Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T18:05:46Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T18:07:46Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-13T18:09:48Z</when>" -- <gx:coord>-2.3668044999999998 51.37418 0</gx:coord>
    , "<when>2016-08-14T05:36:14Z</when>" -- <gx:coord>-0.6563023 51.9895661 0</gx:coord>
    , "<when>2016-08-14T06:41:48Z</when>" -- <gx:coord>-0.1600374 51.5072888 0</gx:coord>
    , "<when>2016-08-14T06:39:45Z</when>" -- <gx:coord>-2.5251558999999997 51.4510597 0</gx:coord>
    , "<when>2016-08-14T06:23:39Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:25:40Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:27:41Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:29:42Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:31:42Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:33:43Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T06:35:43Z</when>" -- <gx:coord>-2.5086481 51.381654100000006 0</gx:coord>
    , "<when>2016-08-14T05:43:42Z</when>" -- <gx:coord>-2.5086483 51.3816537 0</gx:coord>
    , "<when>2016-08-14T05:44:23Z</when>" -- <gx:coord>-2.5086483 51.3816537 0</gx:coord>
    , "<when>2016-08-14T05:46:35Z</when>" -- <gx:coord>-2.5086486 51.381654399999995 0</gx:coord>
    , "<when>2016-08-14T05:48:35Z</when>" -- <gx:coord>-2.5086486 51.381654399999995 0</gx:coord>
    , "<when>2016-08-14T05:50:37Z</when>" -- <gx:coord>-2.5086486 51.381654399999995 0</gx:coord>
    , "<when>2016-08-14T05:52:58Z</when>" -- <gx:coord>-2.5086486 51.381654399999995 0</gx:coord>
    , "<when>2016-08-14T05:59:45Z</when>" -- <gx:coord>-2.5086463 51.3816546 0</gx:coord>
    , "<when>2016-08-14T06:00:58Z</when>" -- <gx:coord>-2.5086463 51.3816546 0</gx:coord>
    , "<when>2016-08-14T06:05:00Z</when>" -- <gx:coord>-2.5086463 51.3816546 0</gx:coord>
    , "<when>2016-08-14T06:09:21Z</when>" -- <gx:coord>-2.5086463 51.3816546 0</gx:coord>
    , "<when>2016-08-14T06:11:22Z</when>" -- <gx:coord>-2.5086463 51.3816546 0</gx:coord>
    , "<when>2016-12-02T16:58:49Z</when>" -- <gx:coord>-3.9386459000000005 56.1543135 0</gx:coord>
    , "<when>2017-02-08T17:30:14Z</when>" -- <gx:coord>12.5635888 55.67328 0</gx:coord>
    , "<when>2017-02-08T17:32:43Z</when>" -- <gx:coord>12.5635891 55.673279799999996 0</gx:coord>
    , "<when>2017-03-01T17:49:49Z</when>" -- <gx:coord>-2.3377282 51.721894 0</gx:coord>
    , "<when>2017-03-10T16:58:51Z</when>" -- <gx:coord>-4.1074934999999995 50.3819919 0</gx:coord>
    , "<when>2017-03-10T17:00:00Z</when>" -- <gx:coord>-4.1074934999999995 50.3819919 0</gx:coord>
    , "<when>2017-03-10T17:20:00Z</when>" -- <gx:coord>55.277161799999995 25.1961458 0</gx:coord>
    , "<when>2017-03-10T17:24:59Z</when>" -- <gx:coord>0.5273841 51.2681578 0</gx:coord>
    , "<when>2017-03-10T17:25:15Z</when>" -- <gx:coord>0.5273841 51.2681578 0</gx:coord>
    , "<when>2017-03-16T11:08:45Z</when>" -- <gx:coord>9.8327318 53.5422583 0</gx:coord>
    , "<when>2017-03-16T11:10:13Z</when>" -- <gx:coord>11.2483843 47.383562399999995 0</gx:coord>
    , "<when>2017-03-16T11:12:15Z</when>" -- <gx:coord>11.1504382 47.312118299999995 0</gx:coord>
    , "<when>2017-03-16T11:18:17Z</when>" -- <gx:coord>11.1504382 47.312118299999995 0</gx:coord>
    , "<when>2017-03-19T12:08:13Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:09:16Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:10:16Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:11:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:12:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:13:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:14:00Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:14:40Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:15:20Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:16:00Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:16:40Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:17:42Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:18:42Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:19:43Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:20:51Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:21:51Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:22:51Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:23:52Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:24:53Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:26:00Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:27:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:32:04Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:34:04Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:36:04Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:36:48Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:37:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:37:48Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:38:19Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:38:49Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:39:19Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:39:50Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:40:50Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:41:51Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:42:51Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:43:52Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:44:52Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:45:42Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:46:52Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:48:00Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:49:00Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:50:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:51:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:52:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:53:02Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:54:02Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:55:02Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:56:03Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:57:04Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:58:03Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T12:59:04Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:00:05Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:00:20Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:01:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:01:25Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:01:45Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:02:01Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:02:26Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:02:46Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:03:08Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:03:45Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:04:16Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:05:17Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:06:17Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:07:17Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:08:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:09:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:10:27Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:11:27Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:12:27Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:13:29Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:14:29Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:15:30Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:16:31Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:17:32Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:18:32Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:19:40Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:20:40Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:21:18Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:21:41Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:22:12Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-19T13:22:43Z</when>" -- <gx:coord>11.3964839 47.2640156 0</gx:coord>
    , "<when>2017-03-20T13:17:06Z</when>" -- <gx:coord>0.1553299 52.234578199999994 0</gx:coord>
    , "<when>2017-03-20T13:17:50Z</when>" -- <gx:coord>0.1553299 52.234578199999994 0</gx:coord>
    , "<when>2017-03-22T17:19:54Z</when>" -- <gx:coord>113.56112739999999 22.1422559 0</gx:coord>
    , "<when>2017-04-03T11:26:21Z</when>" -- <gx:coord>2.575224 49.007089699999995 0</gx:coord>
    , "<when>2017-04-20T16:21:10Z</when>" -- <gx:coord>1.3095622999999998 51.3706659 0</gx:coord>
    , "<when>2017-04-20T16:21:26Z</when>" -- <gx:coord>1.3095622999999998 51.3706659 0</gx:coord>
    , "<when>2017-04-20T16:21:45Z</when>" -- <gx:coord>1.3095622999999998 51.3706659 0</gx:coord>
    , "<when>2017-05-07T14:19:13Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T14:19:31Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T14:19:53Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T14:20:12Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T14:22:14Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T14:22:44Z</when>" -- <gx:coord>-80.0859698 26.544778200000003 0</gx:coord>
    , "<when>2017-05-07T15:34:02Z</when>" -- <gx:coord>-4.1434103 50.377396100000006 0</gx:coord>
    , "<when>2017-05-20T15:18:20Z</when>" -- <gx:coord>73.3766418 18.7514398 0</gx:coord>
    , "<when>2017-05-20T15:18:36Z</when>" -- <gx:coord>73.3766418 18.7514398 0</gx:coord>
    , "<when>2017-05-20T15:18:52Z</when>" -- <gx:coord>73.3766418 18.7514398 0</gx:coord>
    , "<when>2017-05-22T12:41:20Z</when>" -- <gx:coord>116.04167819999999 -8.357486699999999 0</gx:coord>
    , "<when>2017-05-22T12:41:37Z</when>" -- <gx:coord>116.04167819999999 -8.357486699999999 0</gx:coord>
    , "<when>2017-06-02T07:19:21Z</when>" -- <gx:coord>138.24363169999998 34.9021134 0</gx:coord>
    , "<when>2017-06-02T07:19:38Z</when>" -- <gx:coord>138.24363169999998 34.9021134 0</gx:coord>
    , "<when>2017-06-02T07:21:51Z</when>" -- <gx:coord>138.24363169999998 34.9021134 0</gx:coord>
    , "<when>2017-06-02T07:22:09Z</when>" -- <gx:coord>138.24363169999998 34.9021134 0</gx:coord>
    , "<when>2017-06-07T16:01:06Z</when>" -- <gx:coord>137.2580477 36.1409392 0</gx:coord>
    , "<when>2017-06-07T16:04:23Z</when>" -- <gx:coord>8.2522466 51.964563 0</gx:coord>
    , "<when>2017-06-07T16:08:14Z</when>" -- <gx:coord>8.2522466 51.964563 0</gx:coord>
    , "<when>2017-06-10T17:39:28Z</when>" -- <gx:coord>-1.8462907999999998 52.45427290000001 0</gx:coord>
    , "<when>2017-06-12T16:27:01Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-12T16:29:13Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-12T16:31:16Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-12T16:33:18Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-12T16:34:04Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-12T16:34:20Z</when>" -- <gx:coord>-2.7074946 53.7550386 0</gx:coord>
    , "<when>2017-06-17T07:50:48Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T07:51:28Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T07:51:45Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T07:52:05Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T07:57:24Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T07:57:40Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:01:49Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:21:43Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:24:45Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:26:46Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:31:25Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:34:06Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T08:54:00Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T09:06:18Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-17T09:11:11Z</when>" -- <gx:coord>-0.11111309999999999 51.5031702 0</gx:coord>
    , "<when>2017-06-26T11:48:56Z</when>" -- <gx:coord>-0.022185 51.504072699999995 0</gx:coord>
    , "<when>2017-06-26T11:48:56Z</when>" -- <gx:coord>-0.022437099999999998 51.5040932 0</gx:coord>
    , "<when>2017-06-27T10:59:17Z</when>" -- <gx:coord>-0.022185 51.504072699999995 0</gx:coord>
    , "<when>2017-06-27T11:59:04Z</when>" -- <gx:coord>-0.022185 51.5040932 0</gx:coord>
    , "<when>2017-06-27T11:59:04Z</when>" -- <gx:coord>-0.0224344 51.5040885 0</gx:coord>
    , "<when>2017-06-27T15:11:16Z</when>" -- <gx:coord>-0.0224344 51.5040885 0</gx:coord>
    , "<when>2017-06-27T16:40:54Z</when>" -- <gx:coord>-0.1818062 51.547697 0</gx:coord>
    , "<when>2017-06-29T12:04:35Z</when>" -- <gx:coord>-0.0219676 51.504391 0</gx:coord>
    , "<when>2017-06-29T14:57:05Z</when>" -- <gx:coord>-0.0220934 51.504134199999996 0</gx:coord>
    , "<when>2017-06-29T14:57:05Z</when>" -- <gx:coord>-0.0219561 51.5043807 0</gx:coord>
    , "<when>2017-06-30T17:26:23Z</when>" -- <gx:coord>-0.1912294 51.548031699999996 0</gx:coord>
    , "<when>2017-07-01T20:08:03Z</when>" -- <gx:coord>-82.17446559999999 42.3906329 0</gx:coord>
    , "<when>2017-07-03T14:05:51Z</when>" -- <gx:coord>-112.02464839999999 41.153933599999995 0</gx:coord>
    , "<when>2017-07-09T16:43:25Z</when>" -- <gx:coord>-79.61813339999999 43.6852473 0</gx:coord>
    , "<when>2017-07-13T20:15:04Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:18:54Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:19:10Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:19:44Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:20:00Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:20:21Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-13T20:22:34Z</when>" -- <gx:coord>18.13918 59.3082571 0</gx:coord>
    , "<when>2017-07-20T19:37:45Z</when>" -- <gx:coord>2.1718322 41.3816255 0</gx:coord>
    , "<when>2017-07-24T08:36:31Z</when>" -- <gx:coord>-79.6257562 43.6872882 0</gx:coord>
    , "<when>2017-07-24T08:36:31Z</when>" -- <gx:coord>-79.6257452 43.687278 0</gx:coord>
    , "<when>2017-07-26T22:38:04Z</when>" -- <gx:coord>-79.6200858 43.6858522 0</gx:coord>
    , "<when>2017-08-12T05:48:32Z</when>" -- <gx:coord>12.533267700000001 42.890276899999996 0</gx:coord>
    , "<when>2017-08-15T10:28:29Z</when>" -- <gx:coord>-79.61926319999999 43.6857306 0</gx:coord>
    , "<when>2017-08-15T10:52:27Z</when>" -- <gx:coord>-79.6182211 43.6853851 0</gx:coord>
    , "<when>2017-08-25T11:40:46Z</when>" -- <gx:coord>-79.61907599999999 43.6855913 0</gx:coord>
    , "<when>2017-08-27T17:16:12Z</when>" -- <gx:coord>-0.0226447 51.505899799999995 0</gx:coord>
    , "<when>2017-08-27T22:33:02Z</when>" -- <gx:coord>-0.07021419999999999 51.5105723 0</gx:coord>
    , "<when>2017-08-28T19:15:29Z</when>" -- <gx:coord>-0.07020459999999999 51.5109613 0</gx:coord>
    , "<when>2017-08-29T01:34:24Z</when>" -- <gx:coord>-79.6225097 43.6856941 0</gx:coord>
    , "<when>2017-08-29T01:34:24Z</when>" -- <gx:coord>-79.6225097 43.6856941 0</gx:coord>
    , "<when>2017-08-29T01:39:00Z</when>" -- <gx:coord>-79.62232329999999 43.6858345 0</gx:coord>
    , "<when>2017-09-18T20:27:42Z</when>" -- <gx:coord>-1.8978838 52.4789105 0</gx:coord>
    , "<when>2017-10-31T09:29:39Z</when>" -- <gx:coord>-115.1786414 36.121967 0</gx:coord>
    , "<when>2017-10-31T09:30:21Z</when>" -- <gx:coord>-115.1786414 36.121967 0</gx:coord>
    , "<when>2017-10-31T09:30:43Z</when>" -- <gx:coord>-115.1786414 36.121967 0</gx:coord>
    , "<when>2017-10-31T10:10:24Z</when>" -- <gx:coord>-81.46671219999999 28.4238185 0</gx:coord>
    , "<when>2017-10-31T10:12:36Z</when>" -- <gx:coord>-81.46671219999999 28.4238185 0</gx:coord>
    , "<when>2017-10-31T10:19:36Z</when>" -- <gx:coord>7.026059999999999 43.5493034 0</gx:coord>
    , "<when>2017-10-31T10:27:01Z</when>" -- <gx:coord>-81.46670150000001 28.4238291 0</gx:coord>
    , "<when>2017-10-31T12:34:32Z</when>" -- <gx:coord>-122.13037979999999 47.6414309 0</gx:coord>
    , "<when>2017-10-31T16:53:33Z</when>" -- <gx:coord>-2.3858577999999997 51.472986999999996 0</gx:coord>
    , "<when>2017-12-22T08:15:32Z</when>" -- <gx:coord>-1.4106074 51.7466473 0</gx:coord>
    , "<when>2021-05-30T09:18:26Z</when>" -- <gx:coord>-0.3105191 51.743907799999995 140</gx:coord>
    , "<when>2021-05-30T09:19:00Z</when>" -- <gx:coord>-0.3105191 51.743907799999995 140</gx:coord>
    , "<when>2021-06-02T12:06:30Z</when>" -- <gx:coord>-0.3105069 51.7439141 140</gx:coord>
    , "<when>2021-06-11T09:42:11Z</when>" -- <gx:coord>-0.31053149999999996 51.7439147 140</gx:coord>
    , "<when>2021-06-11T11:42:26Z</when>" -- <gx:coord>-0.31053149999999996 51.7439147 140</gx:coord>
    , "<when>2021-06-13T12:10:33Z</when>" -- <gx:coord>-0.3105225 51.74390580000001 140</gx:coord>   
    , "<when>2021-06-27T11:19:00Z</when>" -- <gx:coord>-0.3105383 51.743906499999994 140</gx:coord>
    , "<when>2021-06-27T11:19:35Z</when>" -- <gx:coord>-0.3105383 51.743906499999994 140</gx:coord>
    , "<when>2021-06-27T17:07:13Z</when>" -- <gx:coord>-0.3105383 51.743906499999994 140</gx:coord>
    ]

data Arguments = Args
  { filePrefix  :: String        -- prefix to use for output files
  , filterLevel :: Int           -- drop this many locations for each one kept
  , startTime   :: Maybe UTCTime -- restrict locations to those after this time
  , endTime     :: Maybe UTCTime -- restrict locations to those before this time
  } deriving Show

parseUTCTime :: ReadM UTCTime
parseUTCTime = eitherReader $ \s ->
    case parse s of
      Right d -> Right $ UTCTime d 0
      Left _ -> parse s 
  where
    parse :: ISO8601 t => String -> Either String t
    parse = fromError . iso8601ParseM

args :: Parser Arguments
args = Args 
  <$> strOption
          ( long "output-file-prefix"
         <> short 'o'
         <> metavar "FILE"
         <> help "Prefix to use for output files" )
  <*> option auto
          ( long "filter-level"
         <> short 'f'
         <> help "Drop this many locations for each one kept"
         <> showDefault
         <> value 0
         <> metavar "INT" )
  <*> optional (option parseUTCTime
          ( long "start-time"
         <> short 's'
         <> help "restrict locations to those after this time"
         <> metavar "ISO8601" ))
  <*> optional (option parseUTCTime
          ( long "end-time"
         <> short 'e'
         <> help "restrict locations to those before this time"
         <> metavar "ISO8601" ))

arguments :: ParserInfo Arguments
arguments = info (args <**> helper)
  ( fullDesc
  <> progDesc "Save filtered location history to files prefixed with FILE"
  <> header "filter - a program for filtering location history XML files" )

main :: IO ()
main = do
    as@(Args o f s e) <- execParser arguments
    putStrLn $ "Using arguments: " ++ show as
    b <- readFile "body_full.xml" -- Google Location History exported in KML format with Header and Footer manually removed
    let locs = restrict' s e $ map parse $ removeWhen $ lines b
    locs <- removeRepeats <$> printJumps locs        
    let locs' = filterN f locs
        ts (Loc _ _ t) = t
        (Loc _ _ min) = minimumOn ts locs'
        (Loc _ _ max) = maximumOn ts locs'
    outputMulti o locs'
    putStrLn $ "From: " ++ iso8601Show min 
    putStrLn $ "To:   " ++ iso8601Show max  

-- originally the xml files from google were in reverse date order
restrict :: Maybe UTCTime -> Maybe UTCTime -> [Location] -> [Location]
restrict Nothing  Nothing  ls = ls
restrict Nothing  (Just e) ls = dropWhile (\(Loc _ _ w) -> w > e) ls 
restrict (Just s) e        ls = takeWhile (\(Loc _ _ w) -> w >= s) $ restrict Nothing e ls 

-- sometime before 2019-11-02 the xml files have been in date order
restrict' :: Maybe UTCTime -> Maybe UTCTime -> [Location] -> [Location]
restrict' Nothing  Nothing  ls = ls
restrict' (Just s) Nothing  ls = dropWhile (\(Loc _ _ w) -> w < s) ls
restrict' s        (Just e) ls = takeWhile (\(Loc _ _ w) -> w <= e) $ restrict' s Nothing ls 


outputMulti :: String -> [Location] -> IO ()
outputMulti f = outputMulti' 0
  where
    outputMulti' _ [] = return ()
    outputMulti' n locs = do
        let (locs',locs'') = getMaxRows (length h + length t) [] locs        
        output (f ++ '_':show n) locs' 
        outputMulti' (n+1) locs'' 

getMaxRows :: Int -> [String] -> [Location] -> ([String],[Location])
getMaxRows _ next [] = (reverse next,[])
getMaxRows size next (l:ls) = 
    let l' = show l 
        s = size + length l' + 1 in
        if s > 5242880 -- bytes in 5MB
        then (reverse next, l:ls)
        else getMaxRows s (l':next) ls

h :: String
h =    "<?xml version='1.0' encoding='UTF-8'?>" 
    ++ "<kml xmlns='http://www.opengis.net/kml/2.2' xmlns:gx='http://www.google.com/kml/ext/2.2'>"
    ++ "<Document>" 
    ++ "<Placemark>" 
    ++ "<open>1</open>" 
    ++ "<gx:Track>" 
    ++ "<altitudeMode>clampToGround</altitudeMode>\n"

t :: String
t =    "</gx:Track>"
    ++ "</Placemark>"
    ++ "</Document>"
    ++ "</kml>"

output :: String -> [String] -> IO ()
output f locs = do
    let file = f ++ ".kml"
    writeFile file h
    appendFile file $ unlines locs
    appendFile file t   


tag :: String
tag = "<gx:coord>"

ctag :: String
ctag = "</gx:coord>"

wtag :: String
wtag = "<when>"

wctag :: String
wctag = "</when>"

data Location = Loc 
  { locationLatitude  :: Double  -- North/South
  , locationLongitude :: Double  -- East/West 
  , locationTimestamp :: UTCTime
  }

withinDelta :: Location -> Location -> Bool
withinDelta (Loc a1 b1 t1) (Loc a2 b2 t2) = (check a1 a2 && check b1 b2) || t1 == t2
    where
      check x y = abs (x - y) <= 1e-5 

instance Show Location where
  show (Loc a b _) = tag ++ (printf "%.5f" a) ++ " " ++ (printf "%.5f" b) ++ ctag 

newtype Error a = Error { fromError :: Either String a } deriving newtype (Monad,Applicative,Functor)

instance MonadFail Error where
  fail x = Error $ Left x

tryWith :: String -> Error a -> a
tryWith msg (Error a) = either (\e -> error $ msg ++ e) id a 

parse :: (String,String) -> Location
parse (w,x) = Loc (read a) (read b) (tryWith "Failed to parse timestamp: " $ iso8601ParseM w')
  where
    [a,b,_] = words $ drop (length tag) x
    w' = drop (length wtag) $ take (length w - length wctag) w

removeWhen :: [String] -> [(String,String)]
removeWhen (w:g:wgs) = 
    let w' = dropWhile isSpace w in
    if  w' `Set.member` spurious
    then removeWhen wgs 
    else (w',dropWhile isSpace g):removeWhen wgs
removeWhen _ = []

removeRepeats :: [Location] -> [Location]
removeRepeats (g1:g2:gs) = 
  if withinDelta g1 g2 
  then removeRepeats $ g2:gs
  else g1:(removeRepeats $ g2:gs)
removeRepeats ls = ls

printJumps :: [Location] -> IO [Location]
printJumps (g1:g2:g3:gs) = do
  isJump <- printJump g1 g2 g3
  if isJump
  then printJumps (g1:g3:gs)
  else ((:) g1) <$> printJumps (g2:g3:gs)
printJumps end = pure end

-- distance in miles between two locations
greatCircleDistance :: Location -> Location -> Double
greatCircleDistance (Loc lat1D lon1D _) (Loc lat2D lon2D _)
    = earthRadius * centralAngle
  where
    earthRadius = 3958.761 -- miles
    toRadians deg = deg * pi / 180.0
    lat1 = toRadians lat1D
    lon1 = toRadians lon1D
    lat2 = toRadians lat2D
    lon2 = toRadians lon2D
    deltaLon = abs (lon2 - lon1)
    sins = (sin lat1) * (sin lat2)
    coss = (cos lat1) * (cos lat2) * (cos deltaLon)
    centralAngle = acos (sins + coss)

speedBetweenMPH :: Location -> Location -> Double
speedBetweenMPH loc1@(Loc _ _ t1) loc2@(Loc _ _ t2) 
    = distanceMiles / timeHours
  where
    distanceMiles = greatCircleDistance loc1 loc2
    timeSeconds :: Integer
    timeSeconds = floor (diffUTCTime t2 t1)
    timeHours = abs $ fromIntegral timeSeconds / (60.0*60.0)

printJump :: Location -> Location -> Location -> IO Bool
printJump l1 l2 l3 = do
  l1l2 <- printJump' True l1 l2
  if l1l2
  then do
    l2l3 <- printJump' True l2 l3
    if l2l3 
    then do
      l1l3 <- printJump' False l1 l3
      if l1l3
      then pure False
      else pure True
    else pure False  
  else pure False

-- is the jump "unrealistic" and if "very unrealistic" reverseLookup the locations and print the jump
printJump' :: Bool -> Location -> Location -> IO Bool
printJump' actuallyPrint l1@(Loc a1 b1 w1) l2@(Loc a2 b2 w2) = 
    if    distance > 1.0/64.0 -- ~ 25 meters
       && speed > 100 
    then do
      when (   actuallyPrint 
            && distance > 5.0
            && speed > 767.269148 -- speed of sound 
           ) $ do 
        exp <- ex
        putStrLn $ msg exp
      pure True
    else pure False
  where
    distance = greatCircleDistance l1 l2
    speed = speedBetweenMPH l1 l2
    msg ex =    iso8601Show w1 
             ++ " -> " 
             ++ iso8601Show w2 
             ++ " (" ++ ex ++ ")"
             ++ " { " ++ (printf "%.3f" distance) ++ " miles"
             ++ ", at " ++ (printf "%.3f" speed) ++ "MPH }"
    lookupCity l = do
      p <- cachedLookup (locKey l)
      pure $ placeName p
    ex = case Map.lookup (w1,w2) explained of
           Just ex -> pure ex
           Nothing -> do
             c1 <- lookupCity l1
             c2 <- lookupCity l2
             pure $ c1 ++ " to " ++ c2

placeName :: Either GeoError Place -> String
placeName (Left _) = "Unknown"
placeName (Right p)
    -- | Just x <- f "town"      = x
    | Just x <- f "city"      = x
    | Just x <- f "aerodrome" = x
    | Just x <- f "province"  = x
    | Just x <- f "county"    = x
    | Just x <- f "state"     = x
    | Just x <- f "country"   = x
    | otherwise = unpack . displayName $ p
  where
    f k = unpack <$> (Map.lookup k . address $ p)

filterN :: Int -> [a] -> [a]    
filterN x = filter'
  where
    filter' (c:cs) = c:filter' (drop x cs)
    filter' _ = []

explained :: Map.Map (UTCTime,UTCTime) String
explained = Map.fromList $ map f
   [ ("2010-04-03T06:59:29Z","2010-04-03T07:13:25Z","Bristol to Geneva")
   , ("2010-04-10T11:03:58Z","2010-04-10T11:04:14Z","Saas Fee to Bristol")
   , ("2010-04-21T09:39:50Z","2010-04-22T18:43:41Z","Gatwick to Golden Lake")
   , ("2010-04-22T18:43:41Z","2010-04-23T13:52:33Z","Golden Lake to Ottawa")
   , ("2017-09-30T07:20:51Z","2017-09-30T07:44:55Z","Mallorca to Luton (en route)")
   , ("2018-06-01T20:29:28Z","2018-06-01T20:40:28Z","Luton to Stockholm (en route)")
   , ("2019-07-20T18:34:53Z","2019-07-20T18:41:54Z","Heathrow to Ottawa (en route)")
   , ("2019-07-20T19:47:14Z","2019-07-20T19:52:34Z","Heathrow to Ottawa (en route)")
   , ("2019-07-20T20:05:58Z","2019-07-20T20:12:53Z","Heathrow to Ottawa (en route)")
   , ("2019-08-03T23:37:14Z","2019-08-03T23:47:50Z","Ottawa to Heathrow (en route)")
   , ("2019-08-18T13:27:45Z","2019-08-18T13:31:55Z","Luton to Berlin (en route)")
   , ("2019-08-18T13:31:55Z","2019-08-18T13:47:33Z","Luton to Berlin (en route)")
   , ("2019-08-18T13:47:33Z","2019-08-18T13:53:48Z","Luton to Berlin (en route)")
   ]
 where
   p = tryWith "Failed to parse explained: " . iso8601ParseM
   f (t1,t2,x) = ((p t1,p t2),x)

data LocationKey = LocationKey 
  { lkLat :: String 
  , lkLon :: String
  } deriving (Eq,Ord,Generic,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

instance Show LocationKey where
  show (LocationKey a b) = a ++ "," ++ b

locKey :: Location -> LocationKey
locKey (Loc a b _) = LocationKey (printf "%.5f" b) (printf "%.5f" a)

{-
-- open street map API returns JSON
-- e.g. https://nominatim.openstreetmap.org/reverse?format=json&lat=45.55486&lon=-77.47650
-}

test :: IO Place
test = either (error . show) id <$> (cachedLookup $ LocationKey "45.55486" "-77.47650")

test2 :: IO Place
test2 = either (error . show) id <$> (cachedLookup $ LocationKey "59.62111" "-34.81933")

data Place = Place
  { placeId     :: Int
  , licence     :: Text
  , osmType     :: Text
  , osmId       :: Int
  , lat         :: Text
  , lon         :: Text
  , displayName :: Text
  , address     :: Map.Map Text Text --Address
  , boundingbox :: [Text]
  } deriving Show

instance FromJSON Place where
  parseJSON (Object o) =
    Place <$> o .: "place_id"
          <*> o .: "licence"
          <*> o .: "osm_type"
          <*> o .: "osm_id"
          <*> o .: "lat"
          <*> o .: "lon"
          <*> o .: "display_name"
          <*> o .: "address"
          <*> o .: "boundingbox"
  parseJSON _ = mzero

instance ToJSON Place where
  toJSON p = object 
    [ "place_id"     .= placeId p
    , "licence"      .= licence p
    , "osm_type"     .= osmType p
    , "osm_id"       .= osmId p
    , "lat"          .= lat p
    , "lon"          .= lon p
    , "display_name" .= displayName p
    , "address"      .= address p
    , "boundingbox"  .= boundingbox p
    ]

newtype GeoError = GeoError Text deriving Show

instance FromJSON GeoError where
  parseJSON (Object o) =
    GeoError <$> o .: "error"
  parseJSON _ = mzero

{-
data Address = Address
  { road :: Text
  , city :: Text
  , county :: Text
  , stateDistrict :: Text
  , state :: Text
  , postcode :: Text
  , country :: Text
  , countryCode :: Text
  } deriving Show


\"house_number\":\"131\",
\"road\":\"Bloomingdale Street\",
\"neighbourhood\":\"Central Park\",
\"city_district\":\"River\",
\"city\":\"(Old) Ottawa\",
\"state_district\":\"Eastern Ontario\",
\"state\":\"Ontario\",
\"postcode\":\"K2C 4A2\",
\"country\":\"Canada\",
\"country_code\":\"ca\"}

instance FromJSON Address where
  parseJSON (Object o) =
    Address <$> o .: "road"
            <*> o .: "city"
            <*> o .: "county"
            <*> o .: "state_district"
            <*> o .: "state"
            <*> o .: "postcode"
            <*> o .: "country"
            <*> o .: "country_code"
  parseJSON _ = mzero

instance ToJSON Address where
  toJSON a = object 
    [ "road"           .= road a
    , "city"           .= city a
    , "county"         .= county a
    , "state_district" .= stateDistrict a
    , "state"          .= state a
    , "postcode"       .= postcode a
    , "country"        .= country a
    , "country_code"   .= countryCode a
    ]
-}

type Cache = Map.Map LocationKey Place

cacheLookup :: LocationKey -> Cache -> Maybe Place
cacheLookup = Map.lookup

-- definitely not thread safe!!! but also ensures maximum of 1 lookup per second
cachedLookup :: LocationKey -> IO (Either GeoError Place)
cachedLookup lk = do
  let cachefile = "location.cache"
  cache <- either error id <$> eitherDecodeFileStrict' cachefile
  case cacheLookup lk cache of
    Just place -> pure $ Right place
    Nothing -> do
      putStrLn $ "reverseLookup: " ++ show lk
      plc <- reverseLookup lk
      sleep 1.0
      case plc of
        Left err -> pure ()
        Right plc -> encodeFile cachefile $ Map.insert lk plc cache
      pure plc

reverseLookup :: LocationKey -> IO (Either GeoError Place)
reverseLookup (LocationKey a b) = do
  manager <- newManager tlsManagerSettings
  let url = "https://nominatim.openstreetmap.org/reverse"
            ++ "?format=json"
            ++ "&lat=" ++ a 
            ++ "&lon=" ++ b 
            ++ "&email=" ++ userEmail
  request <- parseRequest url
  let request' = request {requestHeaders = [("User-Agent", "Haskell Nominatim Client")]}
  response <- httpLbs request' manager
  let bs = responseBody response
      plc = eitherDecode' bs
  case plc of
    Right plc -> pure $ Right plc
    Left flr -> do
      let err = eitherDecode' bs
      case err of
        Right err -> pure $ Left err
        _ -> error $ flr ++ "\n" ++ show bs
 