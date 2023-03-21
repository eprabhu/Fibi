package com.polus.fibicomp.person.delegation.delegationDao;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.delegation.pojo.DelegationStatus;
import com.polus.fibicomp.person.delegation.pojo.Delegations;

@Transactional
@Service(value = "delegationDao")
public class DelegationDaoImpl implements DelegationDao{

	protected static Logger logger = LogManager.getLogger(DelegationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	private static final String DELEGATION_STATUS_CODE = "delegationStatusCode";

	@Override
	public Delegations saveOrUpdateDelegation(Delegations delegation) {
		try {
			hibernateTemplate.saveOrUpdate(delegation);
		} catch (Exception e) {
			logger.info("Error ocuured in saveOrUpdateDelegation {}", e.getMessage());
		}
		return delegation;
	}

	@Override
	public List<Delegations> getDelegationByPersonId(String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Delegations> query = builder.createQuery(Delegations.class);
			Root<Delegations> rootDelegations = query.from(Delegations.class);
			Predicate predicatePersonId = builder.equal(rootDelegations.get("delegatedTo"), personId);
			Predicate predicateStatus = builder.notEqual(rootDelegations.get(DELEGATION_STATUS_CODE),
					Constants.DELEGATION_DELETE_STATUS);
			query.where(builder.and(predicatePersonId, predicateStatus));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			logger.info("Error ocuured in getDelegationByPersonId {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	@Override
	public Delegations getDelegationByDelegationId(Integer delegationId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Delegations> query = builder.createQuery(Delegations.class);
			Root<Delegations> rootDelegations = query.from(Delegations.class);
			Predicate predicateDelegationId = builder.equal(rootDelegations.get("delegationId"), delegationId);
			query.where(builder.and(predicateDelegationId));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in getDelegationByDelegationId {}", e.getMessage());
			return null;
		}
	}

	@Override
	public Delegations loadDelegationByParams(String personId, List<String> delegationStatusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Delegations> query = builder.createQuery(Delegations.class);
			Root<Delegations> rootDelegations = query.from(Delegations.class);
			Predicate predicateDelegatedBy = builder.equal(rootDelegations.get("delegatedBy"), personId);
			Predicate predicateStatus = rootDelegations.get(DELEGATION_STATUS_CODE).in(delegationStatusCode);
			query.where(builder.and(predicateDelegatedBy, predicateStatus));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in loadDelegationByParams {}", e.getMessage());
			return null;
		}
	}

	@Override
	public DelegationStatus loadDelegationStatusById(String delegationStatusCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<DelegationStatus> query = builder.createQuery(DelegationStatus.class);
			Root<DelegationStatus> rootDelegations = query.from(DelegationStatus.class);
			Predicate predicateStatus = builder.equal(rootDelegations.get(DELEGATION_STATUS_CODE), delegationStatusCode);
			query.where(builder.and(predicateStatus));
			return session.createQuery(query).getSingleResult();
		} catch (Exception e) {
			logger.info("Error ocuured in loadDelegationStatusById {}", e.getMessage());
			return null;
		}
	}

}
