import React, { Component } from 'react';

const INTERVAL = 200;

const MESSAGES  = [
    "Fetching",
    "Fetching.",
    "Fetching..",
    "Fetching...",
];

export default class Fetching extends Component {
    constructor(props) {
        super(props);
        this.state = {
            next: 1,
            message: MESSAGES[0],
        }
    }

    tick() {
        const curr = this.state.next;
        const message = MESSAGES[curr];
        const next = (curr + 1) === MESSAGES.length ? 0 : (curr + 1)
        const newState = {
            next,
            message,
        }
        this.setState(newState)
    }
    
    componentDidMount() {
        this.interval = setInterval(this.tick.bind(this), INTERVAL)
    }

    componentWillUnmount() {
        clearInterval(this.interval)
    }
    
    render() {
        return (
                <div className="Fetching">{this.state.message}</div>
        )
    }
}
