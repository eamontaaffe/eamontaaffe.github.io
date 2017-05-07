import React from 'react';
import { Provider } from 'react-redux';
import { Router, Route, hashHistory } from 'react-router';
import configureStore from '../configureStore';

import MainLayout from './MainLayout'
import App from './App';
import Blog from './Blog';
import AboutContainer from './AboutContainer';
import Books from './Books';
import Podcasts from './Podcasts';

const store = configureStore();

const Root = () => (
	<Provider store={store}>
	<Router history={hashHistory}>
	<Route component={MainLayout}>
	<Route path="/" component={App} />
	<Route path="/blog" component={Blog} />
	<Route path="/about" component={AboutContainer} />
	<Route path="/books" component={Books} />
	<Route path="/podcasts" component={Podcasts} />
	</Route>
	</Router>
	</Provider>
)

export default Root;
