PROJECT = json_validator
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = etv
TEST_DEPS = jiffy jsx

dep_etv = git https://github.com/erlangsters/erlang-term-validator master
dep_jiffy = git https://github.com/davisp/jiffy 1.1.1
dep_jsx = git https://github.com/talentdeficit/jsx v3.1.0

include erlang.mk
