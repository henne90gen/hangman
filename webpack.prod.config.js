const path = require('path');
const webpack = require('webpack');
const CopyPlugin = require('copy-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const InterpolateHtmlPlugin = require('interpolate-html-plugin');
const TerserPlugin = require('terser-webpack-plugin');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');

const publicUrl = 'https://henne90gen.github.io/hangman';
const publicPath = '/hangman/';
const appHtml = './public/index.html';

process.env.NODE_ENV = "production";

module.exports = {
    mode: 'production',
    bail: true,
    entry: './src/index.ts',
    devtool: false,
    plugins: [
        new HtmlWebpackPlugin({
            inject: true,
            template: appHtml,
        }),
        new InterpolateHtmlPlugin({ PUBLIC_URL: publicUrl }),
        new webpack.DefinePlugin({
            'process.env.PUBLIC_URL': JSON.stringify(publicUrl),
        }),
        new CopyPlugin({
            patterns: [
                {
                    from: './public/',
                    to: './',
                    globOptions: {
                        ignore: ['**/index.html'],
                    },
                },
            ],
        }),
        new MiniCssExtractPlugin(),
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
    optimization: {
        minimize: true,
        minimizer: [
            new TerserPlugin({
                terserOptions: {
                    ecma: 2017,
                    compress: {
                        passes: 2,
                        warnings: true,
                        pure_getters: true,
                        keep_fargs: false,
                        unsafe_comps: true,
                        unsafe: true,
                        pure_funcs: [
                            'A2',
                            'A3',
                            'A4',
                            'A5',
                            'A6',
                            'A7',
                            'A8',
                            'A9',
                            'F2',
                            'F3',
                            'F4',
                            'F5',
                            'F6',
                            'F7',
                            'F8',
                            'F9',
                        ],
                    },
                    mangle: {
                        safari10: true,
                    },
                    output: {
                        comments: false,
                        ascii_only: true,
                    },
                },
                extractComments: false,
            }),
            new CssMinimizerPlugin(),
        ],
        splitChunks: {
            chunks: 'all',
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
                    { loader: 'elm-asset-webpack-loader' },
                    {
                        loader: 'elm-webpack-loader',
                        options: {
                            debug: false,
                            optimize: true,
                        },
                    },
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
                use: [
                    MiniCssExtractPlugin.loader,
                    'css-loader',
                    'postcss-loader',
                ],
            },
        ],
    },
};
