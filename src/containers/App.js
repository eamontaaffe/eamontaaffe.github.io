import React, { Component } from 'react';
import { connect } from 'react-redux';
import marked from 'marked';

import Folder from '../components/Folder';
import SubFolder from '../components/SubFolder';
import LastSubFolder from '../components/LastSubFolder';

class App extends Component {
    render() {

	// You can use the aws-sdk package to access s3 bucket objects
	// once you have the bucket objects, you can create the folder list dynamically
	
   return (
	<div className="App" >
	    {"This is Eamon's website..."} <br/>
            {"============================================"} <br/>
            <Folder name="home" /><br/>
            <SubFolder name="about" /><br/>
	    <SubFolder name="blog" /><br/>
	    <SubFolder name="books" /><br/>
	   <LastSubFolder name="podcasts" /><br/>
	</div>
    );
  }
}

function mapStateToProps(state) {
    const { dir } = state
    return { dir }
}

export default connect(mapStateToProps)(App);
