import React, { Component } from 'react';
import { connect } from 'react-redux';
import marked from 'marked';

import Folder from '../components/Folder';
import SubFolder from '../components/SubFolder';
import LastSubFolder from '../components/LastSubFolder';

class App extends Component {

    renderDir(dir, id=1, level=0) {
	const head = dir.find(dir => dir.id === id)
	const contents = head.contents || []

	return contents.reduce((acc, val) =>
			       acc.concat(
				   this.renderDir(dir, val, level+1)),
			       [(<Folder
				 name={head.name}
				 key={head.id}
				 level={level}
				 />)])
	
    }
    
    render() {	
	return (
		<div className="App" >
		{"This is Eamon's website..."} <br/>
		{"============================================"} <br/>
		{this.renderDir(this.props.dir)}
		</div>
	);
    }
}

function mapStateToProps(state) {
    const { dir } = state
    return { dir }
}

export default connect(mapStateToProps)(App);
