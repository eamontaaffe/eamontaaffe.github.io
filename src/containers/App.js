import React, { Component } from 'react';
import { connect } from 'react-redux';

import Directory from '../components/Directory';

class App extends Component {    
    render() {	
	return (
		<div className="App" >
		<Directory dir={this.props.dir}/>
		</div>
	);
    }
}

function mapStateToProps(state) {
    const { dir } = state
    return { dir }
}

export default connect(mapStateToProps)(App);
