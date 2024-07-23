package com.polus.integration.service;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.integration.dto.ProposalRequest;
import com.polus.integration.pojo.FibiCOIConnectDummy;
import com.polus.integration.repository.ProposalResponseRepository;


@Transactional
@Service
public class IntegrationServiceImpl implements IntegrationService {

	protected static Logger logger = LogManager.getLogger(IntegrationServiceImpl.class.getName());

    @Autowired
    private ProposalResponseRepository proposalResponseRepository;

	@Override
	public FibiCOIConnectDummy saveOrUpdateRecievedProposalDetail(ProposalRequest proposalRequest) {
		logger.info("Inside fibi coi connect {}");
		logger.info("CoiProjectTypeCode {}", proposalRequest.getCoiProjectTypeCode());
		logger.info("HomeUnit {}", proposalRequest.getHomeUnit());
		logger.info("ModuleItemKey {}", proposalRequest.getModuleItemKey());
		logger.info("PersonId {}", proposalRequest.getPersonId());
		FibiCOIConnectDummy proposalResponse = new FibiCOIConnectDummy();
		proposalResponse.setPersonId(proposalRequest.getPersonId());
		proposalResponse.setProposalId(proposalRequest.getModuleItemKey());
		proposalResponse.setTypeCode(proposalRequest.getCoiProjectTypeCode());
		proposalResponse.setUnitNumber(proposalRequest.getHomeUnit());
		return proposalResponseRepository.save(proposalResponse);
    }
}
