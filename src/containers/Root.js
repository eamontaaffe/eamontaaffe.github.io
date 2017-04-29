import React from 'react';
import { Provider } from 'react-redux';
import { Router, Route, hashHistory } from 'react-router';
import configureStore from '../configureStore';

import MainLayout from './MainLayout'
import App from './App';
import Blog from './Blog';
import About from './About';
import Books from './Books';

const store = configureStore();

const Root = () => (
	<Provider store={store}>
	<Router history={hashHistory}>
	<Route component={MainLayout}>
	<Route path="/" component={App} />
	<Route path="/blog" component={Blog} />
	<Route path="/about" component={About} />
	<Route path="/books" component={Books} />
	</Route>
	</Router>
	</Provider>
)

export default Root;
