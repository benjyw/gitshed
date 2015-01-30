
scala_library(name='history-rewriter-lib',
              sources=globs('*.scala'),
              dependencies=[
                '3rdparty/jvm:bfg'
              ])

jvm_binary(name='history-rewriter',
           main='com.gitshed.Main',
           dependencies=[':history-rewriter-lib'])