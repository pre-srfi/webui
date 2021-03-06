const path = require('path');

module.exports = {
    mode: 'development',
    entry: './src/helpers.js',
    output: {
        filename: 'helpers.js',
        library: 'helpers',
        path: path.resolve(__dirname, 'dist'),
    },
};
