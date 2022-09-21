package com.polus.fibicomp.externalsystemintegration.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service
public interface SystemIntegrationService {
	
	public String getExternalApiInfo();
	
}
