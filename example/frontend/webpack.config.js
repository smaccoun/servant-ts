const {resolve} = require('path');
const {CheckerPlugin} = require('awesome-typescript-loader');
const webpack = require('webpack');

module.exports = {
    mode: 'development',


    output: {
        filename: "bundle.js",
        path: resolve(__dirname, "dist")
    },

    devtool: "source-map",

    devServer: {
        inline:true,
        hot:true,
        port: 3000
    },

    resolve: {
        extensions: [".ts", ".tsx", ".js", ".json"]
    },

    context: resolve(__dirname, './src'),
    entry: "./index.tsx",
    module: {
        rules: [
            { test: /\.tsx?$/, loader: ['babel-loader', 'awesome-typescript-loader'] },
            {
                enforce: 'pre',
                test: /\.js$/,
                loader: "source-map-loader",
                exclude: [/node_modules/, /build/, /__test__/],
            }
        ]
    },
    plugins: [
        new CheckerPlugin(),
        new webpack.HotModuleReplacementPlugin(), // enable HMR globally
        new webpack.NamedModulesPlugin()
    ],
    externals: {
        "react": "React",
        "react-dom": "ReactDOM"
    }

};
