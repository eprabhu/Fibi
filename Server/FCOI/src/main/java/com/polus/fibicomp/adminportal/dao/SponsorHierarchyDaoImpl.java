package com.polus.fibicomp.adminportal.dao;

import com.polus.fibicomp.adminportal.pojo.SponsorHierarchy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import java.sql.Timestamp;
import java.util.List;

@Transactional
@Service(value = "sponsorHierarchyDao")
public class SponsorHierarchyDaoImpl implements SponsorHierarchyDao{

    @Autowired
    private EntityManager entityManager;

    @Autowired
    private HibernateTemplate hibernateTemplate;

    @Override
    public void saveSponsorHierarchy(SponsorHierarchy sponHiearachy) {
        hibernateTemplate.saveOrUpdate(sponHiearachy);
    }

    @Override
    public void deleteSponsorHierarchyById(Integer sponsorGroupId) {
        hibernateTemplate.delete(hibernateTemplate.load(SponsorHierarchy.class, sponsorGroupId));
    }

    @Override
    public List<SponsorHierarchy> getAllSponsorHierarchyFromRoot() {
        Query query = entityManager.createQuery("SELECT hierarchy FROM SponsorHierarchy hierarchy WHERE " +
                "hierarchy.sponsorRootGroupId IS NULL ORDER BY hierarchy.orderNumber DESC");
        return query.getResultList();
    }

    @Override
    public SponsorHierarchy findSponsorHierarchyBySponsorGroupIdOrderByOrderNumberDesc(Integer sponsorGroupId) {
        Query query = entityManager.createQuery("SELECT hierarchy FROM SponsorHierarchy hierarchy WHERE " +
                "hierarchy.sponsorGroupId = :sponsorGroupId ORDER BY hierarchy.orderNumber DESC");
        query.setParameter("sponsorGroupId", sponsorGroupId);
        return (SponsorHierarchy)query.getSingleResult();
    }

    @Override
    public void updateSponsorHierarchy(String sponsorGroupName, Integer orderNumber, Timestamp updateTimestamp, String updateUser, Integer sponsorGroupId) {
        Query query = entityManager.createQuery("UPDATE SponsorHierarchy hierarchy SET hierarchy.sponsorGroupName = :sponsorGroupName, hierarchy.orderNumber = :orderNumber, " +
        "hierarchy.updateTimestamp = :updateTimestamp, hierarchy.updateUser = :updateUser WHERE hierarchy.sponsorGroupId = :sponsorGroupId");
        query.setParameter("sponsorGroupName", sponsorGroupName);
        query.setParameter("orderNumber", orderNumber);
        query.setParameter("updateTimestamp", updateTimestamp);
        query.setParameter("updateUser", updateUser);
        query.setParameter("sponsorGroupId", sponsorGroupId);
        query.executeUpdate();
    }

    @Override
    public List<Object> findAllSponsorsNotInSponsorHierarchy(Integer sponsorGroupId, String searchWord) {
        Query query = entityManager.createQuery("SELECT sponsor.sponsorCode, sponsor.sponsorName, sponsor.sponsorType.description, sponsor.acronym FROM Sponsor sponsor WHERE " +
                "(sponsor.sponsorCode LIKE :searchWord OR sponsor.sponsorName LIKE :searchWord OR sponsor.acronym LIKE :searchWord) AND " +
                        "sponsor.sponsorCode NOT IN (SELECT hierarchy.sponsorCode FROM SponsorHierarchy hierarchy " +
                        "WHERE hierarchy.sponsorRootGroupId = :sponsorGroupId AND hierarchy.sponsorCode IS NOT NULL)");
        query.setParameter("sponsorGroupId", sponsorGroupId);
        query.setParameter("searchWord", searchWord);
        query.setMaxResults(20);
        return query.getResultList();
    }

    @Override
    public List<Object> findAllDistinctSponsorGroupNameBySearchWord(String searchWord) {
        Query query = entityManager.createQuery("SELECT DISTINCT hierarchy.sponsorGroupName, hierarchy.sponsorGroupId FROM SponsorHierarchy hierarchy WHERE " +
                "hierarchy.sponsorCode IS NULL AND hierarchy.sponsorGroupName LIKE :searchWord");
        query.setParameter("searchWord", searchWord);
        return query.getResultList();
    }

    @Override
    public List<Object> findAllDistinctSponsorGroupName() {
        Query query = entityManager.createQuery("SELECT DISTINCT hierarchy.sponsorGroupName, hierarchy.sponsorGroupId " +
                "FROM SponsorHierarchy hierarchy WHERE hierarchy.sponsorCode IS NULL");
        return query.getResultList();
    }
}
