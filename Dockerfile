FROM haskell:9.2.5-buster as build-stage

RUN mkdir app && mkdir src

COPY stack.yaml stack.yaml.lock telegram-roles-bot.cabal package.yaml README.md ChangeLog.md ./

RUN stack build --only-dependencies

COPY . .

RUN stack --local-bin-path build install

FROM scratch AS export-stage
COPY --from=build-stage /build/telegram-roles-bot-exe /telegram-roles-bot-exe_buster