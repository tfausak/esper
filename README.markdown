# Esper

Esper parses Rocket League replays. It is written in PureScript and runs on
Node.js. It aims to have no runtime dependencies.

1.  `npm install`
2.  `npm run purs compile src/Esper.purs`
3.  `node --eval "require('./output/Esper').main('replays/F811C1D24888015E23B598AD8628C742.replay')()"`
