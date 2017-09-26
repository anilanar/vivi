const { Source, parse } = require('graphql/language');
const fs = require('fs');

const gql = fs.readFileSync('./test.gql');
const source = new Source(gql);
const parsed = parse(source);

console.log(JSON.stringify(parsed));



