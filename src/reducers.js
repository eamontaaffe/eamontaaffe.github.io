
const initialState = {
    dir: [
	{
	    id: 1,
	    name: "home",
	    contents: [2,3,4,5],
	},
	{
	    id: 2,
	    name: "about",
	    contents: [6,7]
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
	},
	{
	    id: 6,
	    name: "subfolder 1",
	},
	{
	    id: 7,
	    name: "subfolder 2",
	    contents: [8]
	},
	{
	    id: 8,
	    name: "florence"
	}
    ]
}

const rootReducer = (state = initialState, action) => {
    return state;
}

export default rootReducer;
