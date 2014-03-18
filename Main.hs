module Main where

import System.Environment
import GenLR

-- 英文を構文解析して生成された構文木を全通りDOTファイルに出力
main = do {
          argv <- getArgs;
          let verbose = "--verbose" `elem` argv in do {
              let trees = generalizedLR englishSentence englishLang in do {
                      outputTrees "dots/parse" trees verbose;
              };
          }
}
