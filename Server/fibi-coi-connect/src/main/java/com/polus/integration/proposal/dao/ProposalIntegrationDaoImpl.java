package com.polus.integration.proposal.dao;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;

@Transactional
@Service
public class ProposalIntegrationDaoImpl implements ProposalIntegrationDao {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public COIIntegrationProposal saveOrUpdateCoiIntegrationProposal(COIIntegrationProposal coiIntegrationProposal) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationProposal);
			return coiIntegrationProposal;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationProposal: {}", e.getMessage(), e);
            return null;
        }
	}

	@Override
	public COIIntegrationProposalPerson saveOrUpdateCoiIntegrationProposalPerson(COIIntegrationProposalPerson coiIntegrationProposalPerson) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationProposalPerson);
			return coiIntegrationProposalPerson;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationProposalPerson: {}", e.getMessage(), e);
            return null;
        }
		
	}

	@Override
	public COIIntegrationPropQuestAns saveOrUpdateCoiIntegrationQuestionnaire(COIIntegrationPropQuestAns coiIntegrationPropQuestAns) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationPropQuestAns);
			return coiIntegrationPropQuestAns;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationQuestionnaire: {}", e.getMessage(), e);
            return null;
        }
	}

}
