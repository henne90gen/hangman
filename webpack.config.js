const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const InterpolateHtmlPlugin = require('interpolate-html-plugin');

const publicPath = '/';
const publicUrl = '"http://localhost:8080"';
const appPublic = './public';
const appHtml = './public/index.html';

module.exports = {
    mode: 'development',
    entry: './src/index.ts',
    devtool: 'inline-source-map',
    plugins: [
        new HtmlWebpackPlugin({
            inject: true,
            template: appHtml,
        }),
        new InterpolateHtmlPlugin({
            PUBLIC_URL:
                'http://localhost:8080' /*TODO make this work with the publicUrl variable*/,
        }),
        new webpack.DefinePlugin({
            'process.env.PUBLIC_URL': publicUrl,
        }),
        new webpack.HotModuleReplacementPlugin(),
    ],
    output: {
        clean: true,
        pathinfo: true,
        path: path.resolve(__dirname, 'dist'),
        filename: 'static/js/[name].[chunkhash:8].js',
        chunkFilename: 'static/js/[name].[chunkhash:8].chunk.js',
        publicPath: publicPath,
        devtoolModuleFilenameTemplate: (info) =>
            path.resolve(info.absoluteResourcePath).replace(/\\/g, '/'),
    },
    devServer: {
        contentBase: appPublic,
        watchContentBase: true,
        hot: true,
        watchOptions: {
            ignored: /node_modules/,
        },
    },
    optimization: {
        splitChunks: {
            chunks: 'all',
            name: 'vendors',
        },
        runtimeChunk: true,
    },
    resolve: {
        extensions: ['.js', '.ts', '.elm'],
        modules: ['node_modules'],
    },
    module: {
        strictExportPresence: true,
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    { loader: 'elm-hot-webpack-loader' },
                    { loader: 'elm-webpack-loader', options: {} },
                ],
            },
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
            {
                test: /\.css$/,
                include: path.resolve(__dirname, 'src'),
                use: ['style-loader', 'css-loader', 'postcss-loader'],
            },
        ],
    },
    performance: false,
};
