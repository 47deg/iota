let args = process.argv.slice(2);
if (args.length < 2) {
    console.log("Expected two args: <input.js> <output.js>")
    process.exit(-1);
}

let inputName = args[0];
let outputName = args[1];

if (inputName.trim() == outputName.trim()) {
    console.log("Two different files need to be specified");
    process.exit(-1);
}

let fs = require('fs');
let raw = JSON.parse(fs.readFileSync(inputName));

let processed = raw.map(function(benchmark) {
    let bits = benchmark.benchmark.match(/(\w+)_(\d+)$/)
    let pm = benchmark.primaryMetric;
    let entry = {
        impl: bits[1],
        n   : parseInt(bits[2]),
        score: {
            value     : pm.score,
            error     : pm.scoreError,
            confidence: pm.scoreConfidence
        }
    };
    return entry;
});

let body = processed.map(function(e) { return "  " + JSON.stringify(e); }).join(",\n")
let res = "[\n" + body + "\n]"

fs.writeFileSync(outputName, res);
