import React from 'react';
import { Provider } from 'react-redux';
import { HashRouter as Router, Route, Switch } from 'react-router-dom';
import configureStore from '../configureStore';

import MainLayout from './MainLayout'
import App from './App';
import Blog from './Blog';
import AboutContainer from './AboutContainer';
import Books from './Books';
import Podcasts from './Podcasts';
import NoMatch from './NoMatch';

const store = configureStore();

const Root = () => (
	<Provider store={store}>
	<Router>
        <MainLayout>
        <Switch>
	<Route exact path="/" component={App} />
	<Route path="/blog" component={Blog} />
	<Route path="/about" component={AboutContainer} />
	<Route path="/books" component={Books} />
	<Route path="/podcasts" component={Podcasts} />
        <Route component={NoMatch} />
        </Switch>
        </MainLayout>
	</Router>
	</Provider>
)

export default Root;
