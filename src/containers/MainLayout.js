import React from 'react';

import Header from '../components/Header'
import Footer from '../components/Footer'

const MainLayout = (props) => {
    const path = props.location.pathname || ""
    const name = path.replace(/\//g,"") || "website"
    const heading = `This is Eamon's ${name}.`
    return (
	    <div className="MainLayout">
	    <Header heading={heading} pathname={props.location.pathname} />
	    {props.children}
            <Footer />
	    </div>
    )
}

export default MainLayout
