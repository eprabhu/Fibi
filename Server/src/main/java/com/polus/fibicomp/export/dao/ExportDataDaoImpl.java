package com.polus.fibicomp.export.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;

@Transactional
@Service(value = "exportDataDao")
public class ExportDataDaoImpl implements ExportDataDao{

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<Award> getAllActiAwards() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder awardBuilder = session.getCriteriaBuilder();
		CriteriaQuery<Award> awardQuery = awardBuilder.createQuery(Award.class);
		Root<Award> rootAward = awardQuery.from(Award.class);
		Predicate awardSequenceStatus = awardBuilder.equal(rootAward.get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_ACTIVE);
		awardQuery.where(awardBuilder.and(awardSequenceStatus));
		return session.createQuery(awardQuery).getResultList();
	}

	@Override
	public List<AwardFundingProposal> getAwardFundingProposal(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder awardFundingProposalBuilder = session.getCriteriaBuilder();
		CriteriaQuery<AwardFundingProposal> awardFundingProposalQuery = awardFundingProposalBuilder.createQuery(AwardFundingProposal.class);
		Root<AwardFundingProposal> rootAwardFundingProposal = awardFundingProposalQuery.from(AwardFundingProposal.class);
		Predicate predicateAwardId = awardFundingProposalBuilder.equal(rootAwardFundingProposal.get("awardId"), awardId);
		awardFundingProposalQuery.where(awardFundingProposalBuilder.and(predicateAwardId));
		return session.createQuery(awardFundingProposalQuery).getResultList();
	}

	@Override
	public InstituteProposalAdminDetail getInstituteProposalAdminDetail(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder ipAdminDetailbuilder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAdminDetail> ipAdminDetailQuery = ipAdminDetailbuilder.createQuery(InstituteProposalAdminDetail.class);
		Root<InstituteProposalAdminDetail> rootIPDetails = ipAdminDetailQuery.from(InstituteProposalAdminDetail.class);
		Predicate predicateOne = ipAdminDetailbuilder.equal(rootIPDetails.get("instProposalId"), proposalId);
		ipAdminDetailQuery.where(ipAdminDetailbuilder.and(predicateOne));
		return session.createQuery(ipAdminDetailQuery).uniqueResult();
	}

}
