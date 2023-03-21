package com.polus.fibicomp.claims.claimsIntegration.sapfeed.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;


@Transactional
@Service(value = "claimInvoiceIntegrationService")
public interface ClaimInvoiceIntegrationService {

	/**
	 * This method is used to generate template for feed
	 * @param feedIds
	 * @param sapFeedMaintenanceVO
	 * @return 
	 */
	public String fastTemplateGeneration(List<Integer> feedIds, SapFeedMaintenanceVO sapFeedMaintenanceVO);

	/**
	 * @param userActionCode 
	 * @return status
	 */
	public String processClaimInvoiceFeedResponse(String userActionCode);
}
