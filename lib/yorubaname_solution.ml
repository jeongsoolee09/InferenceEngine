type solution = Method.t * string list

let yorubaname_udf_solution : solution array =
  [| ("String AboutUsController.aboutUsIndexPage(Model)", ["src"; "sin"])
   ; ("void AbstractNameEntry.setVariants(String)", ["non"])
   ; ("void AbstractNameEntry.setUpdatedAt(LocalDateTime)", ["non"])
   ; ("void AbstractNameEntry.setMeaning(String)", ["non"])
   ; ("void AbstractNameEntry.setIpaNotation(String)", ["non"])
   ; ("void AbstractNameEntry.setState(State)", ["non"])
   ; ("LocalDateTime AbstractNameEntry.getCreatedAt()", ["non"])
   ; ("void AbstractNameEntry.setSyllables(String)", ["non"])
   ; ("void AbstractNameEntry.setExtendedMeaning(String)", ["non"])
   ; ("void AbstractNameEntry.setMedia(String)", ["non"])
   ; ("String AbstractNameEntry.getIpaNotation()", ["non"])
   ; ("void AbstractNameEntry.setTags(String)", ["non"])
   ; ("String AbstractNameEntry.getVariants()", ["non"])
   ; ("void AbstractNameEntry.setInOtherLanguages(String)", ["non"])
   ; ("void AbstractNameEntry.setEtymology(List)", ["non"])
   ; ("String AbstractNameEntry.getFamousPeople()", ["non"])
   ; ("String AbstractNameEntry.getMeaning()", ["non"])
   ; ("String AbstractNameEntry.getTags()", ["non"])
   ; ("List AbstractNameEntry.getEtymology()", ["non"])
   ; ("void AbstractNameEntry.setGeoLocation(List)", ["non"])
   ; ("void AbstractNameEntry.setTonalMark(char[])", ["non"])
   ; ("String AbstractNameEntry.getSyllables()", ["non"])
   ; ("String AbstractNameEntry.getMedia()", ["non"])
   ; ("void AbstractNameEntry.setPronunciation(String)", ["non"])
   ; ("String AbstractNameEntry.getMorphology()", ["non"])
   ; ("List AbstractNameEntry.getGeoLocation()", ["non"])
   ; ("void AbstractNameEntry.setCreatedAt(LocalDateTime)", ["non"])
   ; ("String AbstractNameEntry.getSubmittedBy()", ["non"])
   ; ("char[] AbstractNameEntry.getTonalMark()", ["non"])
   ; ("String AbstractNameEntry.getPronunciation()", ["non"])
   ; ("LocalDateTime AbstractNameEntry.getUpdatedAt()", ["non"])
   ; ("String AbstractNameEntry.getInOtherLanguages()", ["non"])
   ; ("State AbstractNameEntry.getState()", ["non"])
   ; ("String AbstractNameEntry.getExtendedMeaning()", ["non"])
   ; ("void AbstractNameEntry.setMorphology(String)", ["non"])
   ; ("void AbstractNameEntry.setFamousPeople(String)", ["non"])
   ; ("void AbstractNameEntry.setSubmittedBy(String)", ["non"])
   ; ("ResponseEntity AdminApi.listGeoLocations()", ["src"; "sin"])
   ; ("ResponseEntity ApiExceptionHandler.handlerException(GenericApiCallException)", ["non"])
   ; ("Map ApiService.getSearchActivity()", ["non"])
   ; ("List ApiService.getAllNames()", ["non"])
   ; ("Integer ApiService.getIndexedNameCount()", ["non"])
   ; ("NameEntry ApiService.getName(String)", ["non"])
   ; ("void ApiService.setAPIPATH(String)", ["non"])
   ; ("List ApiService.getAllNamesByAlphabet(String)", ["non"])
   ; ("List ApiService.getGeoLocations()", ["non"])
   ; ("List ApiService.searchName(String)", ["non"])
   ; ("void ApiUser.setPassword(String)", ["non"])
   ; ("String ApiUser.getRoles()", ["non"])
   ; ("void ApiUser.setId(Long)", ["non"])
   ; ("Long ApiUser.getId()", ["non"])
   ; ("void ApiUser.setEmail(String)", ["non"])
   ; ("String ApiUser.getEmail()", ["non"])
   ; ("String ApiUser.getUsername()", ["non"])
   ; ("void ApiUser.setUsername(String)", ["non"])
   ; ("String ApiUser.getPassword()", ["non"])
   ; ("void ApiUser.setRoles(String[])", ["non"])
   ; ("UserDetails ApiUserDetailsService.loadUserByUsername(String)", ["non"])
   ; ("void ApiUsersDatabaseImporter.initApiUsers()", [])
   ; ("Map AuthApi.login(Principal)", ["src"; "sin"])
   ; ("List AuthApi.updateUser(UpdateUserRequest,ApiUser)", ["non"])
   ; ("ResponseEntity AuthApi.getAuthMetaData()", ["src"; "sin"])
   ; ("ResponseEntity AuthApi.getUser(Long)", ["src"; "sin"])
   ; ("HashMap AuthApi.response(String)", ["non"])
   ; ("List AuthApi.getUsers()", ["non"])
   ; ("ResponseEntity AuthApi.create(CreateUserRequest,BindingResult)", ["src"; "sin"])
   ; ("ResponseEntity AuthApi.deleteUser(Long,UpdateUserRequest)", ["src"; "sin"])
   ; ("ResponseEntity AuthApi.deleteUser(Long,Principal)", ["src"; "sin"])
   ; ( "void AuthConfiguration.configureGlobal(AuthenticationManagerBuilder,UserDetailsService)"
     , ["non"] )
   ; ("void AuthConfiguration.configure(HttpSecurity)", ["non"])
   ; ("void CORSFilter.doFilter(ServletRequest,ServletResponse,FilterChain)", ["non"])
   ; ("void CORSFilter.destroy()", ["non"])
   ; ("void CORSFilter.init(FilterConfig)", ["non"])
   ; ("BiMap ColumnOrder.getColumnOrder()", ["non"])
   ; ("void ColumnOrder.setOrder(String[])", ["non"])
   ; ("String ColumnOrder.getColumnOrderAsString()", ["non"])
   ; ("String ContactUsController.contactUsIndexPage(Model)", [])
   ; ("List ControllerUtil.getYorubaAlphabets()", ["non"])
   ; ("ArrayList CreateUserRequest.getRoles()", ["non"])
   ; ("String CreateUserRequest.getUsername()", ["non"])
   ; ("String CreateUserRequest.getEmail()", ["non"])
   ; ("String CreateUserRequest.getPassword()", ["non"])
   ; ("String CustomUserDetails.getUsername()", ["non"])
   ; ("boolean CustomUserDetails.isEnabled()", ["non"])
   ; ("boolean CustomUserDetails.isAccountNonLocked()", ["non"])
   ; ("Collection CustomUserDetails.getAuthorities()", ["non"])
   ; ("boolean CustomUserDetails.isAccountNonExpired()", ["non"])
   ; ("boolean CustomUserDetails.isCredentialsNonExpired()", ["non"])
   ; ("void DataImporter.initializeData()", ["non"])
   ; ("void DataImporter.initGeoLocation()", ["non"])
   ; ("List DataImporter.initializeDb()", ["non"])
   ; ("String Diction.getName()", ["non"])
   ; ("String Diction.toString()", ["non"])
   ; ("AudioInputStream Diction.getAudioStream()", ["non"])
   ; ("JavaTimeModule DictionaryApplication.javaTimeModule()", ["non"])
   ; ("CacheManager DictionaryApplication.cacheManager()", ["non"])
   ; ("LocaleChangeInterceptor DictionaryApplication.localeChangeInterceptor()", ["non"])
   ; ("CacheManager DictionaryApplication.ecacheManager()", ["non"])
   ; ("ReloadableResourceBundleMessageSource DictionaryApplication.messageSource()", ["non"])
   ; ("void DictionaryApplication.addInterceptors(InterceptorRegistry)", ["non"])
   ; ("void DictionaryApplication.main(String[])", ["non"])
   ; ("LocaleResolver DictionaryApplication.localeResolver()", ["non"])
   ; ("ContentNegotiatingViewResolver DictionaryApplication.viewResolver()", ["non"])
   ; ("Docket DictionaryApplication.dictionaryApi()", ["non"])
   ; ("void DictionaryApplication.addResourceHandlers(ResourceHandlerRegistry)", ["non"])
   ; ("Long DuplicateNameEntry.getId()", ["non"])
   ; ("String DuplicateNameEntry.getName()", ["non"])
   ; ("void DuplicateNameEntry.setName(String)", ["non"])
   ; ("void ESConfig.setDocumentType(String)", ["non"])
   ; ("String ESConfig.getDocumentType()", ["non"])
   ; ("void ESConfig.setIndexName(String)", ["non"])
   ; ("String ESConfig.getHostName()", ["non"])
   ; ("String ESConfig.getClusterName()", ["non"])
   ; ("String ESConfig.getDataPath()", ["non"])
   ; ("void ESConfig.setDataPath(String)", ["non"])
   ; ("String ESConfig.getIndexName()", ["non"])
   ; ("void ESConfig.setClusterName(String)", ["non"])
   ; ("Integer ESConfig.getPort()", ["non"])
   ; ("void ESConfig.setPort(Integer)", ["non"])
   ; ("void ESConfig.setHostName(String)", ["non"])
   ; ("Class ElasticSearchNodeFactoryBean.getObjectType()", ["non"])
   ; ("Node ElasticSearchNodeFactoryBean.getObject()", ["non"])
   ; ("Object ElasticSearchNodeFactoryBean.getObject()", ["non"])
   ; ("void ElasticSearchNodeFactoryBean.setEsConfig(ESConfig)", ["non"])
   ; ("Node ElasticSearchNodeFactoryBean.getNode()", ["non"])
   ; ("void ElasticSearchNodeFactoryBean.shutDown()", ["non"])
   ; ("boolean ElasticSearchNodeFactoryBean.isSingleton()", ["non"])
   ; ("void ElasticSearchService.buildElasticSearchClient()", ["non"])
   ; ("Set ElasticSearchService.listByAlphabet(String)", ["non"])
   ; ("SearchResponse ElasticSearchService.exactSearchByName(String)", ["non"])
   ; ("boolean ElasticSearchService.isElasticSearchNodeAvailable()", ["non"])
   ; ("IndexOperationStatus ElasticSearchService.indexName(NameEntry)", ["non"])
   ; ("Integer ElasticSearchService.getSearchableNames()", ["non"])
   ; ("NameEntry ElasticSearchService.sourceToNameEntry(Map)", ["non"])
   ; ("DeleteResponse ElasticSearchService.deleteName(String)", ["non"])
   ; ("IndexOperationStatus ElasticSearchService.bulkRemoveFromIndex(List)", ["non"])
   ; ("IndexOperationStatus ElasticSearchService.removeFromIndex(String)", ["non"])
   ; ("void ElasticSearchService.setResourceLoader(ResourceLoader)", ["non"])
   ; ("IndexOperationStatus ElasticSearchService.bulkRemoveByNameFromIndex(List)", ["non"])
   ; ("SearchResponse ElasticSearchService.prefixFilterSearch(String,boolean)", ["non"])
   ; ("IndexOperationStatus ElasticSearchService.bulkIndexName(List)", ["non"])
   ; ("SearchResponse ElasticSearchService.partialSearchByName(String)", ["non"])
   ; ("NameEntry ElasticSearchService.getByName(String)", ["non"])
   ; ("SearchResponse ElasticSearchService.exactSearchByNameAsciiFolded(String)", ["non"])
   ; ("Set ElasticSearchService.search(String)", ["non"])
   ; ("Set ElasticSearchService.autocomplete(String)", ["non"])
   ; ("String Etymology.getMeaning()", ["non"])
   ; ("void Etymology.setPart(String)", ["non"])
   ; ("String Etymology.toString()", ["non"])
   ; ("String Etymology.getPart()", ["non"])
   ; ("void Etymology.setMeaning(String)", ["non"])
   ; ("void EventPubService.registerListeners()", ["non"])
   ; ("void EventPubService.publish(Object[])", ["non"])
   ; ("void ExcelImporter.setColumnOrder(ColumnOrder)", ["non"])
   ; ("void ExcelImporter.setEventPubService(EventPubService)", ["non"])
   ; ("void ExcelImporter.setDuplicateEntryRepository(DuplicateNameEntryRepository)", ["non"])
   ; ("void ExcelImporter.setGeoLocationRepository(GeoLocationRepository)", ["non"])
   ; ("XSSFSheet ExcelImporter.getSheet(File,int)", ["non"])
   ; ("boolean ExcelImporter.alreadyExists(String)", ["non"])
   ; ("ArrayList ExcelImporter.getGeoLocation(String)", ["non"])
   ; ("void ExcelImporter.setNameEntryRepository(NameEntryRepository)", ["non"])
   ; ("void ExcelImporter.setValidator(ImporterValidator)", ["non"])
   ; ("ImportStatus ExcelImporter.importFile(File)", ["non"])
   ; ("ResponseEntity FeedbackApi.getFeedbacks()", ["src"; "sin"])
   ; ("ResponseEntity FeedbackApi.deleteAFeedback(String)", ["src"; "sin"])
   ; ("ResponseEntity FeedbackApi.getAFeedback(String)", ["src"; "sin"])
   ; ("HashMap FeedbackApi.response(String)", ["non"])
   ; ("ResponseEntity FeedbackApi.deleteAllFeedBack()", ["src"; "sin"])
   ; ("ResponseEntity FeedbackApi.getFeedbacksForName(String)", ["src"; "sin"])
   ; ("ResponseEntity FeedbackApi.deleteAllFeedbackForName(String)", ["src"; "sin"])
   ; ("ResponseEntity FeedbackApi.addFeedback(Map)", ["src"; "sin"])
   ; ("void GenericApiCallException.setError(boolean)", ["non"])
   ; ("boolean GenericApiCallException.isError()", ["non"])
   ; ("String GenericApiCallException.getErrorMessage()", ["non"])
   ; ("HttpStatus GenericApiCallException.getStatusResponse()", ["non"])
   ; ("String GeoLocation.getPlace()", ["non"])
   ; ("void GeoLocation.setPlace(String)", ["non"])
   ; ("String GeoLocation.getRegion()", ["non"])
   ; ("String GeoLocation.toString()", ["non"])
   ; ("void GeoLocation.setRegion(String)", ["non"])
   ; ("void GeoLocationTypeConverter.setAsText(String)", ["non"])
   ; ("String GeoLocationTypeConverter.getAsText()", ["non"])
   ; ("String HomeController.indexPage(Model)", ["src"; "sin"])
   ; ("void ImportStatus.setErrorMessages(String)", ["non"])
   ; ("void ImportStatus.incrementNumberOfNames()", ["non"])
   ; ("List ImportStatus.getErrorMessages()", ["non"])
   ; ("Boolean ImportStatus.hasErrors()", ["non"])
   ; ("int ImportStatus.getNumberOfNamesUpload()", ["non"])
   ; ("boolean ImporterValidator.isColumnNameInOrder(XSSFSheet)", ["non"])
   ; ("void ImporterValidator.setColumnOrder(ColumnOrder)", ["non"])
   ; ("String ImproveEntryController.improveEntryIndexPage(Model)", ["src"; "sin"])
   ; ("boolean IndexOperationStatus.getStatus()", ["non"])
   ; ("String IndexOperationStatus.getMessage()", ["non"])
   ; ("IndexOperationStatus JpaSearchService.removeFromIndex(String)", ["non"])
   ; ("IndexOperationStatus JpaSearchService.bulkIndexName(List)", ["non"])
   ; ("Set JpaSearchService.listByAlphabet(String)", ["non"])
   ; ("Set JpaSearchService.search(String)", ["non"])
   ; ("IndexOperationStatus JpaSearchService.bulkRemoveFromIndex(List)", ["non"])
   ; ("Integer JpaSearchService.getSearchableNames()", ["non"])
   ; ("IndexOperationStatus JpaSearchService.bulkRemoveByNameFromIndex(List)", ["non"])
   ; ("NameEntry JpaSearchService.getByName(String)", ["non"])
   ; ("Set JpaSearchService.autocomplete(String)", ["non"])
   ; ("ResponseEntity NameApi.uploadProgress(Optional)", ["src"; "sin"])
   ; ("List NameApi.getAllNames(Optional,Optional,Optional,Optional,Optional)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.deleteName(String)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.updateNames(NameEntry[],BindingResult)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.addName(NameEntry[],BindingResult)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.addName(NameEntry,BindingResult)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.batchDeleteName(String[])", ["src"; "sin"])
   ; ("Object NameApi.getName(Optional,Optional,String)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.updateName(String,NameEntry,BindingResult)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.getMetaData()", ["src"; "sin"])
   ; ("String NameApi.formatErrorMessage(BindingResult)", ["non"])
   ; ("HashMap NameApi.response(String)", ["non"])
   ; ("void NameApi.initBinder(WebDataBinder)", ["src"; "sin"])
   ; ("ResponseEntity NameApi.deleteAllNames()", ["src"; "sin"])
   ; ("void NameApi.publishNamesDeletedEvent(List)", ["non"])
   ; ("ResponseEntity NameApi.upload(MultipartFile)", ["src"; "sin"])
   ; ("String NameDeletedEvent.getName()", ["non"])
   ; ("LocalDateTime NameDeletedEvent.getTimestamp()", ["non"])
   ; ("void NameDeletedEventHandler.listen(NameDeletedEvent)", ["non"])
   ; ("Long NameEntry.getId()", ["non"])
   ; ("int NameEntry.compareTo(NameEntry)", ["non"])
   ; ("int NameEntry.compareTo(Object)", ["non"])
   ; ("void NameEntry.setName(String)", ["non"])
   ; ("void NameEntry.update(NameEntry)", ["non"])
   ; ("String NameEntry.getName()", ["non"])
   ; ("String NameEntryFeedback.getFeedback()", ["non"])
   ; ("String NameEntryFeedback.getName()", ["non"])
   ; ("LocalDateTime NameEntryFeedback.getSubmittedAt()", ["non"])
   ; ("long NameEntryFeedback.getId()", ["non"])
   ; ("List NameEntryService.bulkUpdateNames(List)", ["non"])
   ; ("List NameEntryService.loadByState(Optional,Optional,Optional)", ["non"])
   ; ("List NameEntryService.loadAllNames()", ["non"])
   ; ("void NameEntryService.insertTakingCareOfDuplicates(NameEntry)", ["non"])
   ; ("List NameEntryService.getFeedback(NameEntry)", ["non"])
   ; ("void NameEntryService.deleteNameEntryAndDuplicates(String)", ["non"])
   ; ("NameEntry NameEntryService.loadName(String)", ["non"])
   ; ("void NameEntryService.deleteAllAndDuplicates()", ["non"])
   ; ("void NameEntryService.batchDeleteNameEntryAndDuplicates(List)", ["non"])
   ; ("Long NameEntryService.getNameCount()", ["non"])
   ; ("List NameEntryService.loadAllByState(Optional)", ["non"])
   ; ("boolean NameEntryService.alreadyExists(String)", ["non"])
   ; ("void NameEntryService.bulkInsertTakingCareOfDuplicates(List)", ["non"])
   ; ("List NameEntryService.loadAllNames(Optional,Optional)", ["non"])
   ; ("boolean NameEntryService.namePresentAsVariant(String)", ["non"])
   ; ("List NameEntryService.saveNames(List)", ["non"])
   ; ("NameEntry NameEntryService.saveName(NameEntry)", ["non"])
   ; ("void NameEntryService.deleteInDuplicateEntry(DuplicateNameEntry)", ["non"])
   ; ("NameEntry NameEntryService.updateName(NameEntry,NameEntry)", ["non"])
   ; ("List NameEntryService.loadNameDuplicates(String)", ["non"])
   ; ("String NameIndexedEvent.getName()", ["non"])
   ; ("LocalDateTime NameIndexedEvent.getTimestamp()", ["non"])
   ; ("void NameIndexedEventHandler.listen(NameIndexedEvent)", ["non"])
   ; ("String NameSearchedEvent.getNameSearched()", ["non"])
   ; ("String NameSearchedEvent.getIpOfRequest()", ["non"])
   ; ("LocalDateTime NameSearchedEvent.getTimestamp()", ["non"])
   ; ("void NameSearchedEventHandler.listen(NameSearchedEvent)", ["non"])
   ; ("Integer NameUploadStatus.getTotalUploaded()", ["non"])
   ; ("Integer NameUploadStatus.getTotalNumberOfNames()", ["non"])
   ; ("void NameUploadStatus.setStatus(NameUploadedEvent)", ["non"])
   ; ("boolean NameUploadStatus.isUploading()", ["non"])
   ; ("void NameUploadedEvent.setTotalUploaded(int)", ["non"])
   ; ("boolean NameUploadedEvent.isUploading()", ["non"])
   ; ("void NameUploadedEvent.setTotalNumberOfNames(int)", ["non"])
   ; ("int NameUploadedEvent.getTotalNumberOfNames()", ["non"])
   ; ("int NameUploadedEvent.getTotalUploaded()", ["non"])
   ; ("void NameUploadedEvent.isUploading(boolean)", ["non"])
   ; ("void NameUploadedEventHandler.listen(NameUploadedEvent)", [])
   ; ("String NoEntryFoundController.noEntryFoundIndexPage(Model)", ["non"])
   ; ("void RecentIndexes.setLimit(int)", ["non"])
   ; ("void RecentIndexes.stack(String)", ["non"])
   ; ("boolean RecentIndexes.remove(String)", ["non"])
   ; ("String[] RecentIndexes.get()", ["non"])
   ; ("void RecentSearches.updateFrequency(String)", ["non"])
   ; ("void RecentSearches.setPopularListLimit(int)", ["non"])
   ; ("String[] RecentSearches.get()", ["non"])
   ; ("void RecentSearches.setRecencyLimit(int)", ["non"])
   ; ("List RecentSearches.getNameWithSearchFrequency()", ["non"])
   ; ("String[] RecentSearches.getMostPopular()", ["non"])
   ; ("void RecentSearches.insert(String)", ["non"])
   ; ("List RecentSearches.getSearchFrequency()", ["non"])
   ; ("void RecentSearches.stack(String)", ["non"])
   ; ("boolean RecentSearches.remove(String)", ["non"])
   ; ("Role Role.valueOf(String)", ["non"])
   ; ("Role[] Role.values()", ["non"])
   ; ("Set SearchApi.getByAlphabet(Optional)", ["src"; "sin"])
   ; ("void SearchApi.publishNameIsIndexed(NameEntry)", ["src"; "sin"])
   ; ("Map SearchApi.allActivity()", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.returnStatusMessage(List,IndexOperationStatus)", ["non"])
   ; ("Set SearchApi.search(String,HttpServletRequest)", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.getMetaData()", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.indexEntryByName(String)", ["src"; "sin"])
   ; ("String[] SearchApi.recentSearches(String,HttpServletResponse)", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.indexEntry(NameEntry)", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.deleteFromIndex(String)", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.batchIndexEntriesByName(String[])", ["src"; "sin"])
   ; ("Set SearchApi.getAutocomplete(Optional)", ["src"; "sin"])
   ; ("NameEntry SearchApi.findByName(String,HttpServletRequest)", ["src"; "sin"])
   ; ("ResponseEntity SearchApi.batchDeleteFromIndex(String[])", ["src"; "sin"])
   ; ("String SearchResultController.showEntry(String,Model)", ["src"; "sin"])
   ; ("String SearchResultController.alphabeticListing(String,Model)", ["src"; "sin"])
   ; ("List SearchResultController.addAlphabetsToModel(Model)", ["src"; "sin"])
   ; ("String SearchResultController.showAll(Model)", ["src"; "sin"])
   ; ( "String SearchResultController.searchNameQuery(String,Model,RedirectAttributes)"
     , ["src"; "sin"] )
   ; ("Boolean SearchResultController.isEqualWithoutAccent(String,String)", ["non"])
   ; ("State State.valueOf(String)", ["non"])
   ; ("State[] State.values()", ["non"])
   ; ("String SubmitNewNameController.submitNameIndexPage(Model,String)", ["non"])
   ; ("String SuggestedName.getName()", ["non"])
   ; ("String SuggestedName.getEmail()", ["non"])
   ; ("List SuggestedName.getGeoLocation()", ["non"])
   ; ("String SuggestedName.getDetails()", ["non"])
   ; ("Long SuggestedName.getId()", ["non"])
   ; ("List SuggestionApi.getAllSuggestedNames()", ["src"; "sin"])
   ; ("ResponseEntity SuggestionApi.getSuggestedMetaData()", ["src"; "sin"])
   ; ("ResponseEntity SuggestionApi.deleteSuggestedName(Long)", ["src"; "sin"])
   ; ("String SuggestionApi.formatErrorMessage(BindingResult)", ["non"])
   ; ("ResponseEntity SuggestionApi.deleteAllSuggestions()", ["src"; "sin"])
   ; ("ResponseEntity SuggestionApi.suggestName(SuggestedName,BindingResult)", ["src"; "sin"])
   ; ("HashMap SuggestionApi.response(String)", ["non"])
   ; ("String TeamController.aboutUsIndexPage(Model)", ["src"; "sin"])
   ; ("CharSequence UnicodeNormalizer.normalize(String)", ["non"])
   ; ("String UpdateUserRequest.getUsername()", ["non"])
   ; ("String UpdateUserRequest.getPassword()", ["non"])
   ; ("ArrayList UpdateUserRequest.getRoles()", ["non"])
   ; ("String VolunteerController.volunteerIndexPage(ModelMap)", ["src"; "sin"])
   ; ("CharSequence YearHelper.year()", ["non"]) |]


let yorubaname_api_solution : solution array =
  [| ("Collection AbstractAuthenticationToken.getAuthorities()", [])
   ; ( "AbstractDaoAuthenticationConfigurer \
        AbstractDaoAuthenticationConfigurer.passwordEncoder(PasswordEncoder)"
     , [] )
   ; ("Object AbstractRequestMatcherRegistry.antMatchers(HttpMethod,String[])", [])
   ; ("boolean AccessibleObject.isAnnotationPresent(Class)", [])
   ; ("boolean AcknowledgedResponse.isAcknowledged()", [])
   ; ("ListenableActionFuture ActionRequestBuilder.execute()", [])
   ; ("IndicesAdminClient AdminClient.indices()", [])
   ; ("ApiSelectorBuilder ApiSelectorBuilder.apis(Predicate)", [])
   ; ("Docket ApiSelectorBuilder.build()", [])
   ; ("long ApiUserRepository.count()", [])
   ; ("void ApiUserRepository.delete(Serializable)", [])
   ; ("List ApiUserRepository.findAll()", ["src"])
   ; ("Object ApiUserRepository.findOne(Serializable)", ["src"])
   ; ("Object ApiUserRepository.save(Object)", ["sin"])
   ; ("Object ApplicationContext.getBean(Class)", [])
   ; ("boolean ArrayList.add(Object)", ["non"])
   ; ("boolean ArrayList.removeIf(Predicate)", ["non"])
   ; ("int ArrayList.size()", ["non"])
   ; ("Object[] ArrayList.toArray(Object[])", ["non"])
   ; ("List Arrays.asList(Object[])", ["non"])
   ; ("Stream Arrays.stream(Object[])", ["non"])
   ; ("void Assert.state(boolean,String)", ["san"])
   ; ( "DaoAuthenticationConfigurer \
        AuthenticationManagerBuilder.userDetailsService(UserDetailsService)"
     , [] )
   ; ("String BCryptPasswordEncoder.encode(CharSequence)", [])
   ; ("String BeanDefinition.getBeanClassName()", [])
   ; ("void BeanUtils.copyProperties(Object,Object)", [])
   ; ("Object BiMap.get(Object)", [])
   ; ("BiMap BiMap.inverse()", [])
   ; ("Set BiMap.keySet()", [])
   ; ("List BindingResult.getFieldErrors()", [])
   ; ("boolean BindingResult.hasErrors()", [])
   ; ("boolean Boolean.booleanValue()", [])
   ; ("Boolean Boolean.valueOf(boolean)", [])
   ; ("BulkRequestBuilder BulkRequestBuilder.add(DeleteRequestBuilder)", [])
   ; ("BulkRequestBuilder BulkRequestBuilder.add(IndexRequestBuilder)", [])
   ; ("String BulkResponse.buildFailureMessage()", [])
   ; ("boolean BulkResponse.hasFailures()", [])
   ; ("void CacheConfiguration.setEternal(boolean)", [])
   ; ("void CacheConfiguration.setMaxEntriesLocalHeap(long)", [])
   ; ("void CacheConfiguration.setName(String)", [])
   ; ("void CacheConfiguration.setTimeToIdleSeconds(long)", [])
   ; ("CacheManager CacheManager.newInstance(Configuration)", [])
   ; ("Method[] Class.getMethods()", [])
   ; ("String Class.getName()", [])
   ; ("void ClassPathScanningCandidateComponentProvider.addIncludeFilter(TypeFilter)", [])
   ; ("Set ClassPathScanningCandidateComponentProvider.findCandidateComponents(String)", [])
   ; ("ClassLoader ClassUtils.getDefaultClassLoader()", [])
   ; ("Class ClassUtils.resolveClassName(String,ClassLoader)", [])
   ; ("AdminClient Client.admin()", [])
   ; ("void Client.close()", [])
   ; ("BulkRequestBuilder Client.prepareBulk()", [])
   ; ("CountRequestBuilder Client.prepareCount(String[])", [])
   ; ("DeleteRequestBuilder Client.prepareDelete(String,String,String)", [])
   ; ("IndexRequestBuilder Client.prepareIndex(String,String,String)", [])
   ; ("SearchRequestBuilder Client.prepareSearch(String[])", [])
   ; ("boolean Collection.removeIf(Predicate)", ["non"])
   ; ("Stream Collection.stream()", ["non"])
   ; ("List Collections.emptyList()", ["non"])
   ; ("Set Collections.emptySet()", ["non"])
   ; ("void Collections.reverse(List)", ["non"])
   ; ("Set Collections.singleton(Object)", ["non"])
   ; ("List Collections.singletonList(Object)", ["non"])
   ; ("Map Collections.unmodifiableMap(Map)", ["non"])
   ; ("Collector Collectors.toCollection(Supplier)", ["non"])
   ; ("Collector Collectors.toList()", ["non"])
   ; ("void Configuration.addCache(CacheConfiguration)", [])
   ; ("void CookieGenerator.setCookieName(String)", [])
   ; ("void CookieLocaleResolver.setDefaultLocale(Locale)", [])
   ; ("CountRequestBuilder CountRequestBuilder.setQuery(QueryBuilder)", [])
   ; ("long CountResponse.getCount()", [])
   ; ("CreateIndexRequestBuilder CreateIndexRequestBuilder.addMapping(String,String)", [])
   ; ("CreateIndexRequestBuilder CreateIndexRequestBuilder.setSettings(String)", [])
   ; ("HttpSecurityBuilder CsrfConfigurer.disable()", [])
   ; ("void DataBinder.registerCustomEditor(Class,PropertyEditor)", [])
   ; ("String DefaultMessageSourceResolvable.getDefaultMessage()", [])
   ; ("boolean DeleteResponse.isFound()", [])
   ; ("void Deque.addFirst(Object)", ["non"])
   ; ("boolean Deque.contains(Object)", ["non"])
   ; ("Object Deque.remove()", ["non"])
   ; ("boolean Deque.remove(Object)", ["non"])
   ; ("Object Deque.removeLast()", ["non"])
   ; ("int Deque.size()", ["non"])
   ; ("Object[] Deque.toArray(Object[])", ["non"])
   ; ("Docket Docket.apiInfo(ApiInfo)", [])
   ; ("ApiSelectorBuilder Docket.select()", [])
   ; ("void DuplicateNameEntryRepository.delete(Object)", [])
   ; ("void DuplicateNameEntryRepository.deleteAll()", [])
   ; ("void DuplicateNameEntryRepository.flush()", [])
   ; ("Object DuplicateNameEntryRepository.save(Object)", [])
   ; ("boolean Enum.equals(Object)", ["non"])
   ; ("String Enum.toString()", ["non"])
   ; ("Enum Enum.valueOf(Class,String)", ["non"])
   ; ("void EventBus.post(Object)", [])
   ; ("void EventBus.register(Object)", [])
   ; ("Future ExecutorService.submit(Runnable)", [])
   ; ("ExecutorService Executors.newCachedThreadPool()", [])
   ; ("ExecutorService Executors.newSingleThreadExecutor()", [])
   ; ( "ExpressionUrlAuthorizationConfigurer$ExpressionInterceptUrlRegistry \
        ExpressionUrlAuthorizationConfigurer$AuthorizedUrl.hasAnyRole(String[])"
     , [] )
   ; ( "ExpressionUrlAuthorizationConfigurer$ExpressionInterceptUrlRegistry \
        ExpressionUrlAuthorizationConfigurer$AuthorizedUrl.hasRole(String)"
     , [] )
   ; ( "ExpressionUrlAuthorizationConfigurer$ExpressionInterceptUrlRegistry \
        ExpressionUrlAuthorizationConfigurer$AuthorizedUrl.permitAll()"
     , [] )
   ; ( "HttpSecurityBuilder \
        ExpressionUrlAuthorizationConfigurer$ExpressionInterceptUrlRegistry.and()"
     , [] )
   ; ("String FieldError.getField()", [])
   ; ("File File.createTempFile(String,String)", [])
   ; ("boolean File.delete()", [])
   ; ("File File.getAbsoluteFile()", [])
   ; ("byte[] FileCopyUtils.copyToByteArray(InputStream)", [])
   ; ("PrefixFilterBuilder FilterBuilders.prefixFilter(String,String)", [])
   ; ("TermFilterBuilder FilterBuilders.termFilter(String,String)", [])
   ; ("void FilterChain.doFilter(ServletRequest,ServletResponse)", [])
   ; ("List GeoLocationRepository.findAll()", [])
   ; ("Object GeoLocationRepository.save(Object)", [])
   ; ("String GrantedAuthority.getAuthority()", [])
   ; ("Object HashMap.get(Object)", [])
   ; ("Object HashMap.put(Object,Object)", [])
   ; ("int HashMap.size()", [])
   ; ( "ExpressionUrlAuthorizationConfigurer$ExpressionInterceptUrlRegistry \
        HttpSecurity.authorizeRequests()"
     , [] )
   ; ("CsrfConfigurer HttpSecurity.csrf()", [])
   ; ("HttpBasicConfigurer HttpSecurity.httpBasic()", [])
   ; ("String HttpServletRequest.getRemoteAddr()", [])
   ; ("void HttpServletResponse.sendRedirect(String)", [])
   ; ("void HttpServletResponse.setHeader(String,String)", [])
   ; ("byte[] IOUtils.readFully(InputStream,int,boolean)", [])
   ; ("ImmutableBiMap ImmutableBiMap.copyOf(Map)", [])
   ; ("ImmutableSettings$Builder ImmutableSettings.settingsBuilder()", [])
   ; ("Settings ImmutableSettings$Builder.build()", [])
   ; ("ImmutableSettings$Builder ImmutableSettings$Builder.put(String,boolean)", [])
   ; ("ImmutableSettings$Builder ImmutableSettings$Builder.put(String,String)", [])
   ; ("IndexRequestBuilder IndexRequestBuilder.setSource(String)", [])
   ; ("CreateIndexRequestBuilder IndicesAdminClient.prepareCreate(String)", [])
   ; ("IndicesExistsRequestBuilder IndicesAdminClient.prepareExists(String[])", [])
   ; ("boolean IndicesExistsResponse.isExists()", [])
   ; ("int Integer.compare(int,int)", ["non"])
   ; ("int Integer.intValue()", ["non"])
   ; ("long Integer.longValue()", ["non"])
   ; ("Integer Integer.valueOf(int)", ["non"])
   ; ("InterceptorRegistration InterceptorRegistry.addInterceptor(HandlerInterceptor)", [])
   ; ("void Iterable.forEach(Consumer)", ["non"])
   ; ("boolean Iterator.hasNext()", ["non"])
   ; ("Object Iterator.next()", ["non"])
   ; ("void List.add(int,Object)", ["non"])
   ; ("boolean List.add(Object)", ["non"])
   ; ("Object List.get(int)", ["non"])
   ; ("Iterator List.iterator()", ["non"])
   ; ("boolean List.remove(Object)", ["non"])
   ; ("boolean List.removeAll(Collection)", ["non"])
   ; ("int List.size()", ["non"])
   ; ("List List.subList(int,int)", ["non"])
   ; ("Object[] List.toArray(Object[])", ["non"])
   ; ("Object ListenableActionFuture.actionGet()", ["non"])
   ; ("LocalDateTime LocalDateTime.now()", ["src"])
   ; ("void LocaleChangeInterceptor.setParamName(String)", [])
   ; ("void Logger.debug(String,Object,Object)", ["sin"])
   ; ("void Logger.debug(String,Throwable)", ["sin"])
   ; ("void Logger.error(String,Object,Object)", ["sin"])
   ; ("void Logger.error(String,Throwable)", ["sin"])
   ; ("void Logger.info(String)", ["sin"])
   ; ("void Logger.info(String,Object)", ["sin"])
   ; ("void Logger.info(String,Object,Object)", ["sin"])
   ; ("void Logger.info(String,Throwable)", ["sin"])
   ; ("void Logger.info(String,Object[])", ["sin"])
   ; ("void Logger.warn(String,Object)", ["sin"])
   ; ("Logger LoggerFactory.getLogger(Class)", [])
   ; ("Long Long.valueOf(long)", ["non"])
   ; ("Long Long.valueOf(String)", ["non"])
   ; ("boolean Map.containsKey(Object)", [])
   ; ("Object Map.get(Object)", ["non"])
   ; ("Set Map.keySet()", ["non"])
   ; ("Object Map.put(Object,Object)", ["non"])
   ; ("int Math.toIntExact(long)", ["non"])
   ; ("Model Model.addAttribute(String,Object)", ["non"])
   ; ("boolean Model.containsAttribute(String)", ["non"])
   ; ("ModelMap ModelMap.addAttribute(String,Object)", ["non"])
   ; ("boolean MultipartFile.isEmpty()", [])
   ; ("void MultipartFile.transferTo(File)", [])
   ; ("void NameEntryFeedbackRepository.delete(Object)", ["sin"])
   ; ("void NameEntryFeedbackRepository.delete(Serializable)", ["sin"])
   ; ("void NameEntryFeedbackRepository.deleteAll()", ["sin"])
   ; ("Object NameEntryFeedbackRepository.findOne(Serializable)", ["src"])
   ; ("Object NameEntryFeedbackRepository.save(Object)", ["sin"])
   ; ("long NameEntryRepository.count()", [])
   ; ("void NameEntryRepository.delete(Object)", [])
   ; ("void NameEntryRepository.deleteAll()", [])
   ; ("List NameEntryRepository.findAll()", [])
   ; ("Page NameEntryRepository.findAll(Pageable)", [])
   ; ("void NameEntryRepository.flush()", [])
   ; ("List NameEntryRepository.save(Iterable)", [])
   ; ("Object NameEntryRepository.save(Object)", [])
   ; ("Client Node.client()", [])
   ; ("void Node.close()", [])
   ; ("boolean Node.isClosed()", [])
   ; ("NodeBuilder NodeBuilder.clusterName(String)", [])
   ; ("NodeBuilder NodeBuilder.data(boolean)", [])
   ; ("NodeBuilder NodeBuilder.local(boolean)", [])
   ; ("Node NodeBuilder.node()", [])
   ; ("NodeBuilder NodeBuilder.nodeBuilder()", [])
   ; ("NodeBuilder NodeBuilder.settings(Settings)", [])
   ; ("String Normalizer.normalize(CharSequence,Normalizer$Form)", [])
   ; ("Object Object.clone()", [])
   ; ("Class Object.getClass()", [])
   ; ("String Object.toString()", [])
   ; ("Object ObjectMapper.readValue(String,Class)", [])
   ; ("String ObjectMapper.writeValueAsString(Object)", [])
   ; ("Object Optional.get()", ["non"])
   ; ("boolean Optional.isPresent()", ["non"])
   ; ("Optional Optional.map(Function)", ["non"])
   ; ("Object Optional.orElse(Object)", ["non"])
   ; ("Object Optional.orElseGet(Supplier)", ["non"])
   ; ("Predicate Predicates.or(Predicate,Predicate)", ["non"])
   ; ("String Principal.getName()", [])
   ; ("Object PropertyEditorSupport.getValue()", ["non"])
   ; ("void PropertyEditorSupport.setValue(Object)", ["non"])
   ; ("MatchAllQueryBuilder QueryBuilders.matchAllQuery()", [])
   ; ("MatchQueryBuilder QueryBuilders.matchQuery(String,Object)", [])
   ; ("MultiMatchQueryBuilder QueryBuilders.multiMatchQuery(Object,String[])", [])
   ; ("RedirectAttributes RedirectAttributes.addFlashAttribute(String,Object)", [])
   ; ("void ReloadableResourceBundleMessageSource.setBasename(String)", [])
   ; ("void ReloadableResourceBundleMessageSource.setDefaultEncoding(String)", [])
   ; ("Predicate RequestHandlerSelectors.basePackage(String)", [])
   ; ("InputStream Resource.getInputStream()", [])
   ; ("ResourceHandlerRegistration ResourceHandlerRegistration.addResourceLocations(String[])", [])
   ; ("ResourceHandlerRegistration ResourceHandlerRegistration.setCachePeriod(Integer)", [])
   ; ("ResourceHandlerRegistration ResourceHandlerRegistry.addResourceHandler(String[])", [])
   ; ("Resource ResourceLoader.getResource(String)", [])
   ; ("Object RestTemplate.getForObject(String,Class,Object[])", ["src"])
   ; ("Cell Row.getCell(int)", [])
   ; ("int Row.getRowNum()", [])
   ; ("Map SearchHit.getSource()", [])
   ; ("SearchHit[] SearchHits.getHits()", [])
   ; ("SearchRequestBuilder SearchRequestBuilder.setPostFilter(FilterBuilder)", [])
   ; ("SearchRequestBuilder SearchRequestBuilder.setQuery(QueryBuilder)", [])
   ; ("SearchRequestBuilder SearchRequestBuilder.setSize(int)", [])
   ; ("SearchHits SearchResponse.getHits()", [])
   ; ("boolean Set.add(Object)", [])
   ; ("boolean Set.addAll(Collection)", [])
   ; ("Iterator Set.iterator()", [])
   ; ("int Set.size()", [])
   ; ("Object[] Set.toArray()", [])
   ; ("void SpringApplication.addListeners(ApplicationListener[])", [])
   ; ("ConfigurableApplicationContext SpringApplication.run(String[])", [])
   ; ("boolean Stream.allMatch(Predicate)", ["non"])
   ; ("boolean Stream.anyMatch(Predicate)", ["non"])
   ; ("Object Stream.collect(Collector)", ["non"])
   ; ("long Stream.count()", ["non"])
   ; ("Stream Stream.filter(Predicate)", ["non"])
   ; ("void Stream.forEach(Consumer)", ["non"])
   ; ("Stream Stream.map(Function)", ["non"])
   ; ("boolean Stream.noneMatch(Predicate)", ["non"])
   ; ("Stream Stream.of(Object[])", ["non"])
   ; ("Stream Stream.peek(Consumer)", ["non"])
   ; ("Stream Stream.sorted(Comparator)", ["non"])
   ; ("Object[] Stream.toArray(IntFunction)", ["non"])
   ; ("int String.compareTo(String)", ["non"])
   ; ("boolean String.contains(CharSequence)", ["non"])
   ; ("boolean String.equals(Object)", ["non"])
   ; ("boolean String.equalsIgnoreCase(String)", ["non"])
   ; ("int String.hashCode()", ["non"])
   ; ("boolean String.isEmpty()", ["non"])
   ; ("String String.join(CharSequence,Iterable)", ["non"])
   ; ("String String.join(CharSequence,CharSequence[])", ["non"])
   ; ("int String.length()", ["non"])
   ; ("String String.replace(CharSequence,CharSequence)", ["non"])
   ; ("String[] String.split(String)", ["non"])
   ; ("boolean String.startsWith(String)", ["non"])
   ; ("String String.substring(int,int)", ["non"])
   ; ("String String.toLowerCase()", ["non"])
   ; ("String String.toUpperCase()", ["non"])
   ; ("String String.trim()", ["non"])
   ; ("StringBuilder StringBuilder.append(Object)", ["non"])
   ; ("StringBuilder StringBuilder.append(String)", ["non"])
   ; ("String StringBuilder.toString()", ["non"])
   ; ("String StringUtils.stripAccents(String)", ["non"])
   ; ("long SuggestedNameRepository.count()", [])
   ; ("void SuggestedNameRepository.delete(Object)", [])
   ; ("void SuggestedNameRepository.deleteAll()", [])
   ; ("List SuggestedNameRepository.findAll()", [])
   ; ("Object SuggestedNameRepository.findOne(Serializable)", [])
   ; ("Object SuggestedNameRepository.save(Object)", [])
   ; ("String Throwable.getMessage()", ["non"])
   ; ("void Throwable.printStackTrace()", ["sin"])
   ; ("String URLEncoder.encode(String,String)", [])
   ; ("UUID UUID.randomUUID()", ["non"])
   ; ("String UUID.toString()", ["non"])
   ; ("Iterator XSSFRow.cellIterator()", ["non"])
   ; ("int XSSFSheet.getPhysicalNumberOfRows()", ["non"])
   ; ("XSSFRow XSSFSheet.getRow(int)", ["non"])
   ; ("Iterator XSSFSheet.rowIterator()", ["non"])
   ; ("XSSFSheet XSSFWorkbook.getSheetAt(int)", ["non"])
   ; ("Year Year.now()", ["non"])
   ; ("String Year.toString()", ["non"]) |]