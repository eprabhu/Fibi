package com.polus.fibicomp.search.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.search.pojo.SearchResult;
import com.polus.fibicomp.search.service.SearchService;

@RestController
public class SearchController {
	protected static Logger logger = LogManager.getLogger(SearchController.class.getName());

	@Autowired
	@Qualifier(value = "searchService")
	private SearchService searchService;

	@RequestMapping(value = "/searchDocument", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String assignedRoleOfPerson(@RequestBody SearchResult vo, HttpServletRequest request, HttpServletResponse response) {
		return searchService.searchDocument(vo.getModuleCode(), vo.getSearchText());
	}

}
