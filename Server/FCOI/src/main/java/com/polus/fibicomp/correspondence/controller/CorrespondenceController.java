package com.polus.fibicomp.correspondence.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.polus.fibicomp.correspondence.dto.CorrespondenceDataBus;
import com.polus.fibicomp.correspondence.service.CorrespondenceService;

@Controller
public class CorrespondenceController {

	@Autowired
	CorrespondenceService correspondenceService;

	@RequestMapping(value = "/generateCorrespondence", method = RequestMethod.POST)
	public ResponseEntity<byte[]> generateCorrespondence(@RequestBody CorrespondenceDataBus correspondenceDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		return correspondenceService.generateCorrespondence(response, correspondenceDataBus);
	}
}
