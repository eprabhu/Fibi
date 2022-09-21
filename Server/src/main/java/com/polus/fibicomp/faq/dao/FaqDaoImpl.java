package com.polus.fibicomp.faq.dao;

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

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.faq.pojo.FaqAttachment;
import com.polus.fibicomp.faq.pojo.FaqCategory;
import com.polus.fibicomp.pojo.Faq;

@Transactional
@Service(value = "faqDao")
public class FaqDaoImpl implements FaqDao {
	protected static Logger logger = LogManager.getLogger(FaqDaoImpl.class.getName());
	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	CommonDao commonDao;

	@Override
	public List<FaqCategory> listFaqCategory() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FaqCategory> query = builder.createQuery(FaqCategory.class);
		Root<FaqCategory> faqCategory = query.from(FaqCategory.class);
		query.orderBy(builder.asc(faqCategory.get("categoryCode")));
		List<FaqCategory> faqCategorys = session.createQuery(query).getResultList();
		return faqCategorys;
	}

	@Override
	public Faq saveOrUpdateFaq(Faq faq) {
		try {
			faq.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			hibernateTemplate.saveOrUpdate(faq);

		} catch (Exception e) {
			e.printStackTrace();
		}
		return faq;
	}

	@Override
	public List<Faq> fetchFaqByParams(Integer categoryCode, Integer subCategoryCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Faq> query = builder.createQuery(Faq.class);
		Root<Faq> rootFaq = query.from(Faq.class);
		Predicate predicate1 = builder.equal(rootFaq.get("categoryCode"), categoryCode);
		Predicate predicate2 = builder.equal(rootFaq.get("subCategoryCode"), subCategoryCode);

		if (subCategoryCode == null) {
			query.where(builder.and(predicate1));
		} else {
			query.where(builder.and(predicate1, predicate2));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public FaqAttachment fetchFaqAttachmentById(Integer faqAttachmentId) {
		return hibernateTemplate.get(FaqAttachment.class, faqAttachmentId);
	}

	@Override
	public List<FaqAttachment> fetchFaqAttachmentByfaqAttachmentId(Integer faqAttachmentId) {
		
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<FaqAttachment> query = builder.createQuery(FaqAttachment.class);
			Root<FaqAttachment>faqAttachment = query.from(FaqAttachment.class);
			Predicate predicateOne = builder.equal(faqAttachment.get("faqAttachmentId"),faqAttachmentId);
			query.where(builder.and(predicateOne));
			List<FaqAttachment> faqAttachments = session.createQuery(query).getResultList();
			return faqAttachments;
		}

	@Override
	public FaqAttachment deleteFaqAttachment(FaqAttachment faqAttachment) {
			try {
				hibernateTemplate.delete(faqAttachment);
			} catch (Exception e) {
				e.printStackTrace();
			}
			return faqAttachment;
		
	}

	@Override
	public Faq fetchFaqById(Integer questionId) {
		return hibernateTemplate.get(Faq.class, questionId);
	}

}
