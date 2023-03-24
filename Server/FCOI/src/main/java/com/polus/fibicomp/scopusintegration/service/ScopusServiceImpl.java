package com.polus.fibicomp.scopusintegration.service;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.security.AuthenticatedUser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpClientErrorException.Forbidden;
import org.springframework.web.client.HttpClientErrorException.Unauthorized;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.scopusintegration.dao.ScopusDao;
import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.scopusintegration.pojo.Scopus;
import com.polus.fibicomp.scopusintegration.pojo.ScopusAffiliation;
import com.polus.fibicomp.scopusintegration.pojo.ScopusAuthors;
import com.polus.fibicomp.scopusintegration.pojo.ScopusMetrics;
import com.polus.fibicomp.scopusintegration.vo.Metrics;
import com.polus.fibicomp.scopusintegration.vo.Results;
import com.polus.fibicomp.scopusintegration.vo.ScopusVO;

@Transactional
@Configuration
@Service(value = "scopusService")
public class ScopusServiceImpl implements ScopusService {

	protected static Logger logger = LogManager.getLogger(ScopusServiceImpl.class.getName());

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private ScopusDao scopusDao;

	@Autowired
	private AwardService awardService;

	private String scopusAPI;

	private String scivalAPI;
	
	private String scivalAPIExtend1;
	
	private String scivalAPIExtend2;

	private static String FIRSTAUTHOR = "F";

	private static String LASTAUTHOR = "L";

	private static String SINGLEAUTHOR = "S";

	private static String OTHERAUTHOR = "O";
	
	private static String updateUser = "akantony";
	
	private static Boolean isFirstTime = Boolean.TRUE;

	@SuppressWarnings("unchecked")
	@Override
	public void fetchScopusAPIResponse() {
		if (commonDao.isDynSubSectionEnabled(Constants.SCOPUS_DYN_SUBSECTION_CODE)) {
			try {
				logger.info("scopus_integration service execution");
				getScopusConfurationData();
				//calling the API first time 
				Map<String, Object> result = callScopusApi(scopusAPI);
				Map<String, Object> searchResults = (Map<String, Object>) result.get("search-results");
				List<Object> scopusEntrys = (List<Object>) searchResults.get("entry");
				List<Object> links = (List<Object>) searchResults.get("link");
				String totalResults  = searchResults.get("opensearch:totalResults").toString();
				logger.info("Scopus api total Results : {}", totalResults);
				String itemsPerPage  = searchResults.get("opensearch:itemsPerPage").toString();
				Integer count = Integer.parseInt(totalResults)/Integer.parseInt(itemsPerPage);
				//adding first entry
				if (scopusEntrys != null)
				addScopusEntry(scopusEntrys);
				if (links != null && links.size() == 3) { 
					HashMap<String, Object> nextApi = (HashMap<String, Object>) links.get(2);
					scopusAPI = nextApi.get("@href").toString();
				}
				//calling remaining Scopus API's
				for (int i = 1; i <= count; i++) {
					Map<String, Object> resultIteration = callScopusApi(scopusAPI);
					if(resultIteration == null) {
						logger.info("Scopus api failed and returned null");
						return;
					}
					Map<String, Object> searchResultsIteration = (Map<String, Object>) resultIteration.get("search-results");
					scopusEntrys = (List<Object>) searchResultsIteration.get("entry");
					List<Object> linksIteration = (List<Object>) searchResultsIteration.get("link");
					if(linksIteration != null && linksIteration.size() == 3) {
						HashMap<String, Object> nextApi = (HashMap<String, Object>) linksIteration.get(2);
						scopusAPI = nextApi.get("@href").toString();
					}
					logger.info("Scopus api counter {}", i);
					if (scopusEntrys != null && !scopusEntrys.isEmpty()) {
						addScopusEntry(scopusEntrys);
					} else {
						logger.info("Scopus Entrys are empty/null");
					}
				}
				logger.info("Scopus table insertion completed");
			} catch (Exception e) {
				logger.error("Scopus error occured {}", e);
			}
		}
	}

	private Map<String, Object> callScopusApi(String scopusAPI) {
		try {
			URL url = new URL(scopusAPI);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.setRequestProperty("Accept", "application/json");
			if (conn.getResponseCode() != 200) {
				throw new HttpClientErrorException(HttpStatus.valueOf(conn.getResponseCode()), "Execption occured when calling scopus API");
			}
			BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));
			String output = br.readLine();
			ObjectMapper objectMapper = new ObjectMapper();
			TypeReference<HashMap<String,Object>> typeRef = new TypeReference<HashMap<String,Object>>() {};
			return objectMapper.readValue(output, typeRef);
		} catch (Forbidden | Unauthorized e) {
			logger.error("Scopus error Forbidden/Unauthorized occured {}", e);
		} catch (HttpClientErrorException e) {
			if (Boolean.TRUE.equals(isFirstTime)) {
				logger.warn("Scopus error HttpClientErrorException occured retrying again, error -{}", e);
				isFirstTime = false;
				return callScopusApi(scopusAPI);
			} else {
				logger.error("Scopus error HttpClientErrorException occured {}", e);
			}
		} catch (Exception e) {
			logger.error("Scopus error occured {}", e);
		}
		return null;
		
	}

	@SuppressWarnings("unchecked")
	private void addScopusEntry(List<Object> scopusEntrys) {
		if (!scopusEntrys.isEmpty()) {
			scopusEntrys.stream().forEach(scopusEntry -> {
				String scopusId = null;
				String scopusTitle = null;
				String scopusIdData = null;
				String creator = null;
				String issn = null;
				String sourceType = null;
				String citations = null;
				String doi = null;
				String sourceTitle = null;
				String coverDate = null;
				String pubMedId = null;
//				String coverDisplayDate = null;
				String description = null;
				ScopusVO scopusVo = new ScopusVO();
				Scopus scopus = new Scopus();
				HashMap<String, Object> hmResult = (HashMap<String, Object>) scopusEntry;
				if (hmResult.get("dc:title") != null) {
					scopusTitle = hmResult.get("dc:title").toString();
				}
				if (hmResult.get("dc:identifier") != null) {
					scopusIdData = hmResult.get("dc:identifier").toString();
					if (scopusIdData != null && !scopusIdData.isEmpty() && !scopusIdData.equalsIgnoreCase("null")) {
//						logger.info("scopusID : {}", scopusIdData);
						String fullID[] = scopusIdData.split(":");
						scopusId = fullID[1];
						scopus.setScopusId(scopusId);
						scopusVo.setScopusID(scopusId);
					}
				}
				if (hmResult.get("dc:creator") != null) {
					creator = hmResult.get("dc:creator").toString();
				}
				if (hmResult.get("prism:issn") != null) {
					issn = hmResult.get("prism:issn").toString();
				}
				if (hmResult.get("prism:aggregationType") != null) {
					sourceType = hmResult.get("prism:aggregationType").toString();
				}
				if (hmResult.get("citedby-count") != null) {
					citations = hmResult.get("citedby-count").toString();
				}
				if (hmResult.get("prism:doi") != null) {
					doi = hmResult.get("prism:doi").toString();
				}
				if (hmResult.get("prism:publicationName") != null) {
					sourceTitle = hmResult.get("prism:publicationName").toString();
				}
				if (hmResult.get("prism:coverDate") != null) {
					coverDate = hmResult.get("prism:coverDate").toString();
				}
				if (hmResult.get("dc:description") != null) {
					description = hmResult.get("dc:description").toString();
				}
				if (hmResult.get("pubmed-id") != null) {
					pubMedId = hmResult.get("pubmed-id").toString();
				}
				if (scopusTitle != null && !scopusTitle.isEmpty() && !scopusTitle.equalsIgnoreCase("null")) {
					scopus.setTitle(scopusTitle);
				}
				if (sourceTitle != null && !sourceTitle.isEmpty() && !sourceTitle.equalsIgnoreCase("null")) {
					scopus.setSourceTitle(sourceTitle);
				}
				if (creator != null && !creator.isEmpty() && !creator.equalsIgnoreCase("null")) {
					scopus.setCreator(creator);
					scopusVo.setScopusCreator(creator);
				}
				if (issn != null && !issn.isEmpty() && !issn.equalsIgnoreCase("null")) {
					scopus.setIssn(issn);
				}
				if (sourceType != null && !sourceType.isEmpty() && !sourceType.equalsIgnoreCase("null")) {
					scopus.setSourceType(sourceType);
					scopus.setPublicationType(sourceType);
				}
				if (citations != null && !citations.isEmpty() && !citations.equalsIgnoreCase("null")) {
					scopus.setCitations(Integer.valueOf(citations));
				}
				if (doi != null && !doi.isEmpty() && !doi.equalsIgnoreCase("null")) {
					scopus.setDoi(doi);
				}
				if (coverDate != null && !coverDate.isEmpty() && !coverDate.equalsIgnoreCase("null")) {
					scopus.setCoverDate(coverDate);
				}
				/*if (coverDisplayDate != null && !coverDisplayDate.isEmpty() && !coverDisplayDate.equalsIgnoreCase("null")) {
					scopus.setCoverDisplayDate(coverDisplayDate);
				}*/
				if (description != null && !description.isEmpty() && !description.equalsIgnoreCase("null")) {
					scopus.setDescription(description);
				}
				if (pubMedId != null && !pubMedId.isEmpty() && !pubMedId.equalsIgnoreCase("null")) {
					scopus.setPubMedId(pubMedId);
				}
				scopus.setUpdateUser(updateUser);
				if (hmResult.get("link") != null) {
					List<Object> links = (List<Object>) hmResult.get("link");
					if (!links.isEmpty()) {
						scopusVo.setLinks(links);
						scopusVo = getLinks(scopusVo);
						scopus.setReference(scopusVo.getScopusReference());
					}
				}
				scopusVo.setScopus(scopus);
				if (hmResult.get("author") != null) {
					List<Object> authors = (List<Object>) hmResult.get("author");
					if (!authors.isEmpty()) {
						scopusVo.setAuthors(authors);
						scopusVo = getAuthors(scopusVo);
						scopus.setScopusAuthors(scopusVo.getScopusAuthors());
					}
				}
				if (hmResult.get("affiliation") != null) {
					List<Object> affiliations = (List<Object>) hmResult.get("affiliation");
					if (!affiliations.isEmpty()) {
						scopusVo.setAffiliations(affiliations);
						scopusVo = getAffiliations(scopusVo);
						scopus.setScopusAffiliations(scopusVo.getScopusAffiliations());
					}
				}
				//scopusVo = callMetrics(scopusVo);
				//scopus.setScopusMetrics(scopusVo.getScopusMetrics());
				// saveScopus
				scopusDao.saveOrUpdateScopusInfo(scopus);
			});
		}
		logger.info("Scopus table entry");
	}

	@SuppressWarnings("unchecked")
	private ScopusVO getAffiliations(ScopusVO scopusVo) {
		List<Object> affiliations = scopusVo.getAffiliations();
		List<ScopusAffiliation> scopusAffiliations = new ArrayList<>();
		if (affiliations != null && !affiliations.isEmpty()) {
			affiliations.stream().forEach(affiliation -> {
				ScopusAffiliation scopusAffiliation = new ScopusAffiliation();
				HashMap<String, Object> hmaffiliationsResult = (HashMap<String, Object>) affiliation;
				String affiliationId;
				String affiliationName;
				String affiliationCountry;
				String affiliationCity;
				String affiliationUrl;
				// Scopus Affiliation
				if (hmaffiliationsResult.get("afid") != null) {
					affiliationId = hmaffiliationsResult.get("afid").toString();
					if (affiliationId != null && !affiliationId.isEmpty()) {
						scopusAffiliation.setScopusAffId(affiliationId);
					}
				}
				if (hmaffiliationsResult.get("affilname") != null) {
					affiliationName = hmaffiliationsResult.get("affilname").toString();
					if (affiliationName != null && !affiliationName.isEmpty()) {
						scopusAffiliation.setName(affiliationName);
					}
				}
				if (hmaffiliationsResult.get("affiliation-city") != null) {
					affiliationCity = hmaffiliationsResult.get("affiliation-city").toString();
					if (affiliationCity != null && !affiliationCity.isEmpty()) {
						scopusAffiliation.setCity(affiliationCity);
					}
				}
				if (hmaffiliationsResult.get("affiliation-country") != null) {
					affiliationCountry = hmaffiliationsResult.get("affiliation-country").toString();
					if (affiliationCountry != null && !affiliationCountry.isEmpty()) {
						scopusAffiliation.setCountry(affiliationCountry);
					}
				}
				if (hmaffiliationsResult.get("affiliation-url") != null) {
					affiliationUrl = hmaffiliationsResult.get("affiliation-url").toString();
					if (affiliationUrl != null && !affiliationUrl.isEmpty()) {
						scopusAffiliation.setUrl(affiliationUrl);
					}
				}
				scopusAffiliation.setScopus(scopusVo.getScopus());
				scopusAffiliation.setUpdateUser(updateUser);
				scopusAffiliations.add(scopusAffiliation);
			});
			scopusVo.setScopusAffiliations(scopusAffiliations);
		}
		return scopusVo;
	}

	@SuppressWarnings("unchecked")
	private ScopusVO getAuthors(ScopusVO scopusVo) {
		List<Object> authors = scopusVo.getAuthors();
		List<ScopusAuthors> scopusAuthors = new ArrayList<>();
		if (authors != null && !authors.isEmpty()) {
			// Number of authors
			Integer authorsCount = authors.size();
			scopusVo.setAuthorsCount(authorsCount);
			authors.stream().forEach(author -> {
				ScopusAuthors scopusAuthor = new ScopusAuthors();
				String authId;
				HashMap<String, Object> hmAuthorsResult = (HashMap<String, Object>) author;
				// Scopus Author id
				if (hmAuthorsResult.get("authid") != null) {
					authId = hmAuthorsResult.get("authid").toString();
					if (authId != null && !authId.isEmpty()) {
						scopusAuthor.setScopusAuthorId(authId);
					}
				}
				if (hmAuthorsResult.get("authname") != null) {
					String authFirstName = hmAuthorsResult.get("authname").toString();
					if (authFirstName != null && !authFirstName.isEmpty()) {
						scopusAuthor.setAuthorName(authFirstName);
					}
				}
				if (hmAuthorsResult.get("surname") != null) {
					String authSurName = hmAuthorsResult.get("surname").toString();
					if (authSurName != null && !authSurName.isEmpty()) {
						scopusAuthor.setSurName(authSurName);
					}
				}
				if (hmAuthorsResult.get("given-name") != null) {
					String authGivenName = hmAuthorsResult.get("given-name").toString();
					if (authGivenName != null && !authGivenName.isEmpty()) {
						scopusAuthor.setGivenName(authGivenName);
					}
				}
				if (hmAuthorsResult.get("author-url") != null) {
					String authorUrl = hmAuthorsResult.get("author-url").toString();
					if (authorUrl != null && !authorUrl.isEmpty()) {
						scopusAuthor.setAuthorUrl(authorUrl);
					}
				}
				if (hmAuthorsResult.get("initials") != null) {
					String authorIntitials = hmAuthorsResult.get("initials").toString();
					if (authorIntitials != null && !authorIntitials.isEmpty()) {
						scopusAuthor.setInitial(authorIntitials);
					}
				}
				String authorFlag = getAuthorFlag(scopusVo, scopusAuthor, authors, authorsCount);
				scopusAuthor.setAuthorFlag(authorFlag);
				scopusAuthor.setScopus(scopusVo.getScopus());
				scopusAuthor.setUpdateUser(updateUser);
				scopusAuthors.add(scopusAuthor);
			});
			scopusVo.setScopusAuthors(scopusAuthors);
		}
		return scopusVo;
	}

	@SuppressWarnings("unchecked")
	private String getAuthorFlag(ScopusVO scopusVo, ScopusAuthors scopusAuthor, List<Object> authors, int countOfAuthors) {
		String creator = scopusVo.getScopusCreator();
		String firstAuthorName = null;
		String lastAuthorName = null;
		if (countOfAuthors > 1) {
			HashMap<String, Object> hmResultFirstAuthor = (HashMap<String, Object>) authors.get(0);
			if (hmResultFirstAuthor.get("authname") != null) {
				firstAuthorName = hmResultFirstAuthor.get("authname").toString();
			}
			HashMap<String, Object> hmResultLastAuthor = (HashMap<String, Object>) authors.get(countOfAuthors - 1);
			if (hmResultLastAuthor.get("authname") != null) {
				lastAuthorName = hmResultLastAuthor.get("authname").toString();
			}
			if (firstAuthorName != null && firstAuthorName.equalsIgnoreCase(creator)
					&& scopusAuthor.getAuthorName().equalsIgnoreCase(creator) && countOfAuthors > 1) {
				return FIRSTAUTHOR;
			} else if (lastAuthorName != null && lastAuthorName.equalsIgnoreCase(creator)) {
				return LASTAUTHOR;
			}
		} else if (countOfAuthors == 1) {
			return SINGLEAUTHOR;
		}
		return OTHERAUTHOR;
	}

	@SuppressWarnings("unchecked")
	public ScopusVO getLinks(ScopusVO scopusVo) {
		List<Object> links = scopusVo.getLinks();
		if (links != null && !links.isEmpty()) {
			links.stream().forEach(link -> {
				String hRef = null;
				HashMap<String, Object> hmLinkResult = (HashMap<String, Object>) link;
				String ref = hmLinkResult.get("@ref").toString();
				if ((ref != null && !ref.isEmpty() && ref.equalsIgnoreCase("scopus")) && (hmLinkResult.get("@href") != null)) {
					hRef = hmLinkResult.get("@href").toString();
					if (hRef != null && !hRef.isEmpty()) {
						scopusVo.setScopusReference(hRef);
					}
				}
			});
		}
		return scopusVo;
	}

	private ScopusVO callMetrics(ScopusVO scopusVo) {
		try {
			logger.info("scival_integration metrics API execution");
			StringBuilder stringBuilderAPI = new StringBuilder(scivalAPI);
			stringBuilderAPI.append(scivalAPIExtend1);
			stringBuilderAPI.append(scopusVo.getScopusID());
			stringBuilderAPI.append(scivalAPIExtend2);
//			stringBuilderAPI.append("metricTypes=FieldWeightedCitationImpact&publicationIds=" + scopusVo.getScopusID()
//					+ "&yearRange=5yrs&byYear=true&includedDocs=AllPublicationTypes&journalImpactType=CiteScore&showAsFieldWeighted=false&apiKey=7f59af901d2d86f78a1fd60c1bf9426a");
			URL url = new URL(stringBuilderAPI.toString());
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.setRequestProperty("Accept", "application/json");
			if (conn.getResponseCode() != 200) {
				throw new RuntimeException("Failed : HTTP error code : " + conn.getResponseCode());
			}
			BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));
			String output = br.readLine();
			ObjectMapper mapper = new ObjectMapper();
			mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
			ScopusVO vo = mapper.readValue(output, ScopusVO.class);
			List<Results> results = vo.getResults();
			List<ScopusMetrics> scopusMetrics = new ArrayList<>();
			if (results != null && !results.isEmpty()) {
				for (Results result : results) {
					List<Metrics> metrics = result.getMetrics();
					if (metrics != null && !metrics.isEmpty()) {
						metrics.stream().forEach(metric -> {
							Map<Integer, Integer> valueByYear = metric.getValueByYear();
							Set<Integer> year = valueByYear.keySet();
							for (Integer yearKey : year) {
								Integer value = valueByYear.get(yearKey);
								if (value != null) {
									ScopusMetrics scopusMetric = new ScopusMetrics();
									scopusMetric.setScopusId(scopusVo.getScopusID());
									scopusMetric.setScopus(scopusVo.getScopus());
									scopusMetric.setMetricType(metric.getMetricType());
									scopusMetric.setUpdateUser(updateUser);
									scopusMetric.setYear(yearKey);
									scopusMetric.setValue(value);
									scopusMetrics.add(scopusMetric);
								}
							}
						});
					}
				}
			}
			scopusVo.setScopusMetrics(scopusMetrics);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return scopusVo;
	}

	@Override
	public String findScopus(String searchString) {
		return commonDao.convertObjectToJSON(scopusDao.findScopus(searchString));
	}

	@Override
	public String saveAwardScopus(ScopusVO vo) {
		vo.getAwardScopus().setUpdateUser(AuthenticatedUser.getLoginUserName());
		AwardScopus awardScopus = scopusDao.saveOrUpdateAwardScopus(vo.getAwardScopus());
		awardService.updateAwardDocumentUpdateUserAndTimestamp(vo.getAwardScopus().getAwardId(), vo.getAwardScopus().getUpdateUser());
		return commonDao.convertObjectToJSON(awardScopus);
	}

	@Override
	public String deleteAwardScopus(ScopusVO vo) {
		AwardScopus awardScopus = scopusDao.getAwardScopusBasedOnId(Integer.parseInt(vo.getAwardScopusId()));
		vo.setAwardId(awardScopus.getAwardId());
		scopusDao.deleteAwardScopus(awardScopus);
		awardService.updateAwardDocumentUpdateUserAndTimestamp(awardScopus.getAwardId(), AuthenticatedUser.getLoginUserName());
		return loadAllAwardScopus(vo);
	}

	@Override
	public String loadAllAwardScopus(ScopusVO vo) {
		vo.setAwardScopuses(scopusDao.fetchAllAwardScopus(vo.getAwardId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public void getScopusConfurationData() {
		scopusAPI = scopusDao.getConfigurationValue(Constants.SCOPUS_API);
		scivalAPI = scopusDao.getConfigurationValue(Constants.SCOPUS_SCIVAL_API);
		scivalAPIExtend1 = scopusDao.getConfigurationValue(Constants.SCOPUS_SCIVAL_API_EXTEND1);
		scivalAPIExtend2 = scopusDao.getConfigurationValue(Constants.SCOPUS_SCIVAL_API_EXTEND2);
	}
}
