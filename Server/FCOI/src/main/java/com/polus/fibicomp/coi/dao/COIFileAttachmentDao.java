package com.polus.fibicomp.coi.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Root;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.dao.CommonDao;
import org.hibernate.Session;
import org.hibernate.internal.SessionImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;


import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.exception.COIFileAttachmentException;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.repository.COIDisclosureAttachmentRepository;
import com.polus.fibicomp.constants.Constants;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dto.PersonAttachmentDto;
import com.polus.fibicomp.coi.pojo.Attachments;

@Component
public class COIFileAttachmentDao {

	@Autowired
	COIDisclosureAttachmentRepository coiAttachmentRepository;

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private CommonDao commonDao;

    @Transactional(rollbackFor = {COIFileAttachmentException.class})
	public void saveDisclosureAttachmentDetail(COIFileRequestDto request) {
		try {
			hibernateTemplate.saveOrUpdate(DisclAttachment.builder()
					   .attachmentNumber(getNextAttachmentNumber())
					   .versionNumber(1)
					   .attaStatusCode(request.getAttaStatusCode())
					   .attaTypeCode(request.getAttaTypeCode())
					   .commentId(request.getCommentId())
					   .componentReferenceId(request.getComponentReferenceId())
					   .componentReferenceNumber(request.getComponentReferenceNumber())
					   .componentTypeCode(request.getComponentTypeCode())
					   .fileDataId(request.getFileDataId())
					   .fileName(request.getFile().getOriginalFilename())
					   .mimeType(request.getFile().getContentType())
					   .description(request.getDescription())
					   .documentOwnerPersonId(request.getDocumentOwnerPersonId())
					   .updateUser(null)
					   .updateTimestamp(null)
					   .build());
		}catch(Exception e) {
			throw new COIFileAttachmentException("Error at saveDisclosureAttachmentDetail in COIFileAttachmentDao" + e.getMessage());
		}	
	}
	
	@Transactional(rollbackFor = {COIFileAttachmentException.class})
	public Attachments saveAttachmentDetails(PersonAttachmentDto request, String fileDataId) {
		try {
			Attachments attachment = Attachments.builder()
			.attachmentId(request.getAttachmentId())
			.personId(AuthenticatedUser.getLoginPersonId())
			.attaTypeCode(request.getAttaTypeCode())
			.fileName(request.getFileName())
			.mimeType(request.getMimeType())
			.description(request.getDescription())
			.fileDataId(fileDataId)
			.createUser(AuthenticatedUser.getLoginUserName())
			.createTimestamp(commonDao.getCurrentTimestamp())
			.updateUser(AuthenticatedUser.getLoginUserName())
			.updateTimestamp(commonDao.getCurrentTimestamp())
			.attachmentNumber(request.getAttachmentNumber())
			.versionNumber(request.getVersionNumber())
			.build();
			hibernateTemplate.saveOrUpdate(attachment);
			return attachment;
		} catch(Exception e) {
			throw new COIFileAttachmentException("Error at saveAttachmentDetails in COIFileAttachmentDao" + e.getMessage());
		}	
	}  

	private Integer getNextAttachmentNumber() {
		Session session = hibernateTemplate.getSessionFactory().openSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		Integer attachmentNumber = null;
		try {
			statement = connection.prepareCall("{call GENERATE_ATTACHMENT_NUMBER}");
			statement.execute();
			resultSet = statement.getResultSet();
			if (statement.getMoreResults()) {
				resultSet = statement.getResultSet();
				while (resultSet.next()) {
					if (resultSet.getInt("NEXT_VALUE") == 0) {
						throw new ApplicationException("Unable to acquire lock", Constants.JAVA_ERROR);
					}
					attachmentNumber = resultSet.getInt("NEXT_VALUE");
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
			throw new ApplicationException("Unable to fetch data", e, Constants.JAVA_ERROR);
		}
		return attachmentNumber;
	}    

    public List<DisclAttachment> getDisclAttachByRefId(Integer refId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<DisclAttachment> query = builder.createQuery(DisclAttachment.class);
		Root<DisclAttachment> rootDisclAttachment = query.from(DisclAttachment.class);
		query.where(builder.equal(rootDisclAttachment.get("componentReferenceId"), refId));
		return session.createQuery(query).getResultList();
	}

	public List<DisclAttachment> getDisclAttachByRefIdAndTypeCode(Integer refId, Integer typeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<DisclAttachment> query = builder.createQuery(DisclAttachment.class);
		Root<DisclAttachment> rootDisclAttachment = query.from(DisclAttachment.class);
		query.where(builder.equal(rootDisclAttachment.get("componentReferenceId"), refId),
				builder.equal(rootDisclAttachment.get("componentTypeCode"), typeCode));
		return session.createQuery(query).getResultList();
	}

	public List<DisclAttachment> getDisclAttachByCommentId(Integer commentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<DisclAttachment> query = builder.createQuery(DisclAttachment.class);
		Root<DisclAttachment> rootDisclAttachment = query.from(DisclAttachment.class);
		query.where(builder.equal(rootDisclAttachment.get("commentId"), commentId));
		return session.createQuery(query).getResultList();
	}

	public DisclAttachment getDisclAttachByAttachId(Integer attachmentId) {
		return hibernateTemplate.load(DisclAttachment.class, attachmentId);
	}

    @Transactional(rollbackFor = {COIFileAttachmentException.class})
	public void updateDisclosureAttachmentDetail(DisclAttachment disclAttachment) {
		hibernateTemplate.saveOrUpdate(disclAttachment);
	}

	public List<DisclAttachment> getDisclAttachByAttachIds(List<Integer> attachmentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<DisclAttachment> queryDisclAttachment = builder.createQuery(DisclAttachment.class);
		Root<DisclAttachment> rootDisclAttachment = queryDisclAttachment.from(DisclAttachment.class);
		queryDisclAttachment.where(rootDisclAttachment.get("attachmentId").in(attachmentIds));
		return session.createQuery(queryDisclAttachment).getResultList();
	}

	public void deleteDisclAttachment(Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<DisclAttachment> query = builder.createCriteriaDelete(DisclAttachment.class);
		Root<DisclAttachment> root = query.from(DisclAttachment.class);
		query.where(builder.equal(root.get("attachmentId"), attachmentId));
		session.createQuery(query).executeUpdate();
	}

	public void updateDisclosureAttachmentStatus(String statusCode, Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaUpdate<DisclAttachment> updateQuery = builder.createCriteriaUpdate(DisclAttachment.class);
		Root<DisclAttachment> rootDisclAttachment = updateQuery.from(DisclAttachment.class);
		updateQuery.set(rootDisclAttachment.get("attaStatusCode"), statusCode); 
		updateQuery.where(rootDisclAttachment.get("attachmentId").in(attachmentId));
		session.createQuery(updateQuery).executeUpdate();
	}

}
