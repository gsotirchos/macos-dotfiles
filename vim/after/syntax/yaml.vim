syn match MembOperator "\." containedIn=yamlFlowMappingKey

hi! link yamlBool Statement
hi! link yamlKeyValueDelimiter Normal
hi! link yamlFlowIndicator Normal
hi! link yamlBlockMappingKey Function
hi! link yamlBlockCollectionItemStart MembOperator
