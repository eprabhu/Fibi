package com.polus.fibicomp.correspondence.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.correspondence.dto.CorrespondenceDataBus;

public interface CorrespondenceService {

	ResponseEntity<byte[]> generateCorrespondence(HttpServletResponse response,
			CorrespondenceDataBus correspondenceDataBus);

}
