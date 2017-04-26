import React, { Component } from 'react';
import marked from 'marked';

import Folder from './folder';
import SubFolder from './subfolder';
import LastSubFolder from './lastsubfolder';

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

export default App;
