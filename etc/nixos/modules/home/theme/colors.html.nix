{ colors }:

with colors.withHash;
''
  <html>
    <head>
      <style>
        body {
          font-family: sans-serif;
          margin: 2rem;
        }
        body > div {
          display: flex;
          flex-direction: row;
          flex-wrap: wrap;
          gap: 1em;
        }
        body > div > div {
          align-items: center;
          display: flex;
          height: 5rem;
          justify-content: center;
          width: 5rem;
        }
        #base00 { background-color: ${base00}; color: ${base05}; }
        #base01 { background-color: ${base01}; color: ${base05}; }
        #base02 { background-color: ${base02}; color: ${base05}; }
        #base03 { background-color: ${base03}; color: ${base00}; }
        #base04 { background-color: ${base04}; color: ${base00}; }
        #base05 { background-color: ${base05}; color: ${base00}; }
        #base06 { background-color: ${base06}; color: ${base00}; }
        #base07 { background-color: ${base07}; color: ${base00}; }
        #base08 { background-color: ${base08}; color: ${base00}; }
        #base09 { background-color: ${base09}; color: ${base00}; }
        #base0A { background-color: ${base0A}; color: ${base00}; }
        #base0B { background-color: ${base0B}; color: ${base00}; }
        #base0C { background-color: ${base0C}; color: ${base00}; }
        #base0D { background-color: ${base0D}; color: ${base00}; }
        #base0E { background-color: ${base0E}; color: ${base00}; }
        #base0F { background-color: ${base0F}; color: ${base00}; }
      </style>
    </head>
    <body>
      <h2>Primary colors</h2>
      <div>
        <div id="base00">00</div>
        <div id="base01">01</div>
        <div id="base02">02</div>
        <div id="base03">03</div>
        <div id="base04">04</div>
        <div id="base05">05</div>
        <div id="base06">06</div>
        <div id="base07">07</div>
      </div>
      <h2>Accents</h2>
      <div>
        <div id="base08">08</div>
        <div id="base09">09</div>
        <div id="base0A">0A</div>
        <div id="base0B">0B</div>
        <div id="base0C">0C</div>
        <div id="base0D">0D</div>
        <div id="base0E">0E</div>
        <div id="base0F">0F</div>
      </div>
    </body>
  </html>
''
