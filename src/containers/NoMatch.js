import React from 'react';

import { Link } from 'react-router-dom';

const NoMatch = () => (
        <div className="NoMatch">
        {"Whoops, you must be lost."} Go <Link to="/">home</Link>.
        </div>
);

export default NoMatch;
