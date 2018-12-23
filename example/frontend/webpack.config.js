const path = require('path');
const {CheckerPlugin} = require('awesome-typescript-loader');
const webpack = require('webpack');
const HtmlWebPackPlugin = require("html-webpack-plugin");
const CleanWebpackPlugin = require('clean-webpack-plugin');

const htmlPlugin = new HtmlWebPackPlugin({
    template: "./index.html",
    filename: "./index.html",
    inject:true
});

module.exports = {
    output: {
        filename: "bundled.js",
        path: path.resolve(__dirname, "dist")
    },

    devServer: {
        inline:true,
        hot:true,
        port: 3000,
        disableHostCheck: true,
        headers: {
            'Access-Control-Allow-Origin': '*'
        },
        watchOptions: {
            poll: true
        }
    },

    resolve: {
        extensions: [".ts", ".tsx", ".js", ".json"]
    },
    entry: [path.join(__dirname, '/src/index.tsx')],
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                loader: 'awesome-typescript-loader'
            },
            {
                test: /\.scss$/,
                loader: 'style-loader!css-loader!sass-loader',
            },
            {
                test: /\.(jpe?g|png|gif|svg)$/i,
                loader: 'url-loader',
                options: {
                    limit: 10000,
                },
            },
        ]
    },
    plugins: [
        new CleanWebpackPlugin(['dist']),
        new CheckerPlugin(),
        new webpack.HotModuleReplacementPlugin(), // enable HMR globally
        htmlPlugin
    ]

};
