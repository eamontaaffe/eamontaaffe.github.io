const dir = [
    {
	id: 1,
	name: "/",
	alias: "home/",
	contents: [2,3,4,5],
    },
    {
	id: 2,
	name: "about/",
    },
    {
	id: 3,
	name: "blog/",
    },
    {
	id:4,
	name: "books/",
    },
    {
	id: 5,
	name: "podcasts/",
    },
]

const posts = [
    {
	title: "Luigi execution order",
	date: (new Date(2017,2,13)).toJSON(),
	content: "# Luigi execution order\n" +
	    "Disrupt craft beer twee **wolf**. Keytar XOXO deep v wayfarers. Kale chips poke woke brunch iceland, *bespoke banh mi* cred jianbing man braid ramps iPhone franzen tofu. `Lumbersexual` swag aesthetic, thundercats mixtape letterpress tousled butcher keffiyeh sustainable slow-carb cornhole normcore. Bushwick quinoa kickstarter deep v normcore taxidermy. Fixie cronut glossier, tbh austin health goth small batch irony snackwave fam taxidermy narwhal tilde umami before they sold out. Air plant pabst single-origin coffee, cred authentic blue bottle iPhone twee.\n\n" +

"Fixie stumptown hashtag raclette, tousled truffaut intelligentsia tattooed messenger bag lumbersexual banjo biodiesel tumblr. Lomo kickstarter pour-over, 8-bit messenger bag vexillologist af paleo truffaut coloring book hell of. Pinterest lumbersexual street art, chia scenester pabst everyday carry vape tumeric. Small batch health goth offal meditation raclette, dreamcatcher leggings. Cardigan taxidermy YOLO la croix try-hard authentic. 8-bit food truck sriracha post-ironic retro next level, skateboard kickstarter leggings brunch lyft. Portland lyft blue bottle wolf locavore."
    },
    {
	title: "Elixir GenServer breakdown",
	date: (new Date(2017,2,13)).toJSON(),
	content: "# Elixir GenServer breakdown\n" +
	    "Disrupt craft beer twee wolf. Keytar XOXO deep v wayfarers. Kale chips poke woke brunch iceland, bespoke banh mi cred jianbing man braid ramps iPhone franzen tofu. Lumbersexual swag aesthetic, thundercats mixtape letterpress tousled butcher keffiyeh sustainable slow-carb cornhole normcore. Bushwick quinoa kickstarter deep v normcore taxidermy. Fixie cronut glossier, tbh austin health goth small batch irony snackwave fam taxidermy narwhal tilde umami before they sold out. Air plant pabst single-origin coffee, cred authentic blue bottle iPhone twee.\n\n" +

"```\n# Heading\nThis is some content\n`syntax`\n```\n" +

"Fixie stumptown hashtag raclette, tousled truffaut intelligentsia tattooed messenger bag lumbersexual banjo biodiesel tumblr. Lomo kickstarter pour-over, 8-bit messenger bag vexillologist af paleo truffaut coloring book hell of. Pinterest lumbersexual street art, chia scenester pabst everyday carry vape tumeric. Small batch health goth offal meditation raclette, dreamcatcher leggings. Cardigan taxidermy YOLO la croix try-hard authentic. 8-bit food truck sriracha post-ironic retro next level, skateboard kickstarter leggings brunch lyft. Portland lyft blue bottle wolf locavore."
    },
]

const about = "" +
      "Hi, I'm Eamon.\n\nI'm a software developer out of Melbourne, Australia.\n" +
      "\n\nmail: [eamon@taaffe.com.au](mailto:eamon@taaffe.com.au)<br />" + 
      "linkedin: [Eamon Taaffe](http://linkedin.com/in/eamontaaffe)<br />" +
      "github: [eamontaaffe](http://github.com/eamontaaffe)"

const books = [
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
    {
	title: "The Lean Startup",
	author: "Eric Ries",
	link: "https://www.bookdepository.com/Lean-Startup-Eric-Ries/9780307887894"
    },
    {
	title: "Green Eggs and Ham",
	author: "Dr. Seuss",
	link: "https://www.bookdepository.com/Dr-Seuss---Green-Back-Book-Green-Eggs-and-Ham-Green-Back-Book-Dr-Seuss/9780007158461?ref=grid-view&qid=1493446644292&sr=1-2"
    },
]

const initialState = {
    dir,
    posts,
    about,
    books,
}

const rootReducer = (state = initialState, action) => {
    return state;
}

export default rootReducer;
