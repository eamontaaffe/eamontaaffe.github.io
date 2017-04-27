
const initialState = {
    dir: [
	{
	    id: 1,
	    name: "home",
	    contents: [2,3,4,5],
	},
	{
	    id: 2,
	    name: "about"
	},
	{
	    id: 3,
	    name: "blog",
	},
	{
	    id:4,
	    name: "books",
	},
	{
	    id: 5,
	    name: "podcasts",
	}
    ]
}

const rootReducer = (state = initialState, action) => {
    return state;
}

export default rootReducer;
