# labelSetupUI creates expected HTML output

    Code
      labelSetupUI(ns = shiny::NS("test"), gctFileNames = "file-01.gct")
    Output
      <h4>Assign labels</h4>
      <div class="form-group shiny-input-container small-input">
        <label class="control-label" id="test-Label_file-01.gct-label" for="test-Label_file-01.gct">file-01.gct</label>
        <input id="test-Label_file-01.gct" type="text" class="form-control" value="" placeholder="Proteome or Prot"/>
      </div>

# labelSetupUI creates expected HTML output for multiple inputs

    Code
      labelSetupUI(ns = shiny::NS("test"), gctFileNames = c("file-01.gct",
        "file-02.gct"))
    Output
      <h4>Assign labels</h4>
      <div class="form-group shiny-input-container small-input">
        <label class="control-label" id="test-Label_file-01.gct-label" for="test-Label_file-01.gct">file-01.gct</label>
        <input id="test-Label_file-01.gct" type="text" class="form-control" value="" placeholder="Proteome or Prot"/>
      </div>
      <div class="form-group shiny-input-container small-input">
        <label class="control-label" id="test-Label_file-02.gct-label" for="test-Label_file-02.gct">file-02.gct</label>
        <input id="test-Label_file-02.gct" type="text" class="form-control" value="" placeholder="Proteome or Prot"/>
      </div>

