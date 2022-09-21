package com.polus.fibicomp.support.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;



import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import com.polus.fibicomp.prereview.pojo.PreReview;
import com.polus.fibicomp.prereview.pojo.PreReviewAttachmentFile;

@Transactional
@Service(value = "supportPreReviewDao")
public class SupportDaoImpl implements SupportDao {

	protected static Logger logger = LogManager.getLogger(SupportDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<PreReview> showUnansweredQuestions(String personId,Integer limit) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<PreReview> preReview = new ArrayList<>();
		try {
			statement = connection.prepareCall("{call SUPPORT_WIDGETS(?,?)}");
			statement.setString(1, personId);
			statement.setInt(2, limit);
			statement.execute();
			resultSet = statement.getResultSet();
			while (resultSet != null && resultSet.next()) {
				PreReview preReviews = new PreReview();
				if (resultSet.getString("PRE_REVIEW_ID") != null) {
					preReviews.setPreReviewId((resultSet.getInt("PRE_REVIEW_ID")));
				}
				if (resultSet.getString("MODULE_ITEM_CODE") != null) {
					preReviews.setModuleItemCode((resultSet.getInt("MODULE_ITEM_CODE")));
				}
				if (resultSet.getString("MODULE_ITEM_KEY") != null) {
					preReviews.setModuleItemKey((resultSet.getString("MODULE_ITEM_KEY")));
				}
				if (resultSet.getString("PRE_REVIEW_SECTION_TYPE_CODE") != null) {
					preReviews.setPreReviewId((resultSet.getInt("PRE_REVIEW_SECTION_TYPE_CODE")));
				}
				if (resultSet.getString("REQUESTOR_FULLNAME") != null) {
					preReviews.setRequestorFullName((resultSet.getString("REQUESTOR_FULLNAME")));
				}
				if (resultSet.getString("REQUESTOR_COMMENT") != null) {
					preReviews.setRequestorComment((resultSet.getString("REQUESTOR_COMMENT")));
				}
				if (resultSet.getString("UPDATE_TIMESTAMP") != null) {
					preReviews.setUpdateTimeStamp((resultSet.getTimestamp("UPDATE_TIMESTAMP")));
				}
				if (resultSet.getString("TITLE") != null) {
					preReviews.setModuleItemLabel((resultSet.getString("TITLE")));
				}
				if (resultSet.getString("AGREEMENT_TITLE") != null) {
					preReviews.setModuleItemLabel((resultSet.getString("AGREEMENT_TITLE")));
				}
				if (preReviews.getModuleItemLabel() != null) {
					preReview.add(preReviews);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return preReview;
	}

	@Override
	public PreReviewAttachmentFile getFileDataById(String fileDataId) {
		return hibernateTemplate.get(PreReviewAttachmentFile.class, fileDataId);
	}
}
