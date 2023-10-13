package com.polus.fibicomp.coi.dao;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.faq.dao.FaqDaoImpl;
import com.polus.fibicomp.faq.pojo.FaqCategory;
import com.polus.fibicomp.pojo.Faq;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Repository;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.transaction.Transactional;
import java.util.List;

@Repository
@Transactional
public class FAQDaoImpl implements FAQDao{

    protected static Logger logger = LogManager.getLogger(FAQDaoImpl.class.getName());

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Autowired
    private CommonDao commonDao;

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
    public Faq saveOrUpdateFaq(Faq faq) {
        try {
            faq.setUpdateTimestamp(commonDao.getCurrentTimestamp());
            hibernateTemplate.saveOrUpdate(faq);

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return faq;
    }
}
