package com.polus.fibicomp.agreements.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.dto.AgreementComment;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.pojo.AgreementNoteAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementNoteFileData;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.negotiation.dao.NegotiationAgreementDao;
import com.polus.fibicomp.negotiation.pojo.NegotiationCommentAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationCommentFileData;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.FileData;

@Transactional
@Service(value = "agreementCommentService")
public class AgreementCommentServiceImpl implements AgreementCommentService{

	protected static Logger logger = LogManager.getLogger(AgreementCommentServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private NegotiationAgreementDao negotiationAgreementDao;

	/*@Override
	public String addLocationComment(MultipartFile[] files, String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		AgreementVO vo = null;
		try {
		vo = mapper.readValue(formDataJSON, AgreementVO.class);
		NegotiationsComment negotiationComment = new NegotiationsComment();
		negotiationComment.setComments(vo.getComment());
		negotiationComment.setNegotiationId(vo.getNegotiationId());
		negotiationComment.setNegotiationLocationId(vo.getNegotiationLocationId());
		negotiationComment.setNegotiationsLocation(negotiationAgreementDao.getNegotiationLocationById(vo.getNegotiationLocationId()));
		negotiationComment.setCommentTypeCode("4");
		negotiationComment.setUpdateUser(vo.getUpdateUser());
		negotiationComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		addLocationAttachment(negotiationComment, files);
		negotiationAgreementDao.saveOrUpdateNegotiationsComment(negotiationComment);
		vo.setNegotiationsComments(prepareLocationComments(negotiationAgreementDao.fetchNegotiationCommentBasedOnParams(vo.getNegotiationLocationId())));
		vo.setAgreementComments(prepareAgreementComment(vo.getAgreementRequestId()));
		vo.setNegotiationsLocations(agreementService.prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		AgreementHeader agreementHeader = agreementDao.getAgreementById(vo.getAgreementRequestId());
		agreementHeader.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		agreementDao.saveOrUpdateAgreement(agreementHeader);
		} catch (Exception e) {
			logger.info("Error while adding Location Comment");
		}
		return commonDao.convertObjectToJSON(vo);
	}*/

	/*@Override
	public String deleteAgreementComment(AgreementVO vo) {
		if (Boolean.TRUE.equals(vo.getIsLocationComment())) {
			NegotiationsComment negotiationsComment = negotiationAgreementDao.getNegotiationsCommentById(vo.getCommentId());
			if (negotiationsComment != null) {
				if (negotiationsComment.getNegotiationCommentAttachment() != null && !negotiationsComment.getNegotiationCommentAttachment().isEmpty()) {
					for (NegotiationCommentAttachment negotiationCommentAttachment : negotiationsComment.getNegotiationCommentAttachment()) {
						commonDao.deleteFileData(commonDao.getNegotiationCommentFileDataById(negotiationCommentAttachment.getNegotiationAttachmentFileId()));
					}
					
				}
				negotiationAgreementDao.deleteNegotiationsComment(negotiationsComment);
			}
		} else {
			AgreementNote agreementNote = agreementDao.fetchAgreementNoteById(vo.getCommentId());
			if (agreementNote != null) {
				if (agreementNote.getAgreementNoteAttachment() != null && !agreementNote.getAgreementNoteAttachment().isEmpty()) {
					for (AgreementNoteAttachment agreementNoteAttachment : agreementNote.getAgreementNoteAttachment()) {
						commonDao.deleteFileData(commonDao.getAgreementNoteFileDataById(agreementNoteAttachment.getAgreementNoteFileId()));
					}
				}
				agreementDao.deleteAgreementNote(agreementNote);
			}
		}
		vo.setAgreementComments(prepareAgreementComment(vo.getAgreementRequestId()));
		if (vo.getNegotiationLocationId() != null) {
			vo.setNegotiationsComments(prepareLocationComments(negotiationAgreementDao.fetchNegotiationCommentBasedOnParams(vo.getNegotiationLocationId())));
		}
		if (vo.getNegotiationId() != null) {
			vo.setNegotiationsLocations(agreementService.prepareNegotiationsLocations(negotiationAgreementDao.getLocationByNegotiationId(vo.getNegotiationId())));
		}
		return commonDao.convertObjectToJSON(vo);
	}*/

	/*@Override
	public String addAgreementComment(MultipartFile[] files, String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		AgreementVO vo = null;
		try {
		vo = mapper.readValue(formDataJSON, AgreementVO.class);
		Integer negotiationId = vo.getNegotiationId();
		String updateUser = vo.getUpdateUser();
		String comment = vo.getComment();
		if (comment != null) {
			if (vo.getLocationTypeCode() != null) {
				NegotiationsLocation negotiationsLocation = negotiationAgreementDao.fetchNegotiationLocationBasedOnParams(vo.getNegotiationId(), vo.getLocationTypeCode(), vo.getPersonId());
				if (negotiationsLocation != null) {
					addLocationComment(negotiationsLocation, negotiationId, comment, updateUser, files);
				} else {
					addAgreementComments(vo, files);
				}
			} else {
				addAgreementComments(vo, files);
			}
		}
		} catch (Exception e) {
			e.printStackTrace();
			logger.info("Exception while adding comment");
		}
		if (vo != null && vo.getAgreementRequestId() != null) {
			vo.setAgreementComments(prepareAgreementComment(vo.getAgreementRequestId()));
		}
		return commonDao.convertObjectToJSON(vo);
	}

	private void addAgreementComments(AgreementVO vo, MultipartFile[] files) {
		AgreementNote agreementNote = new AgreementNote();
		agreementNote.setNote(vo.getComment());
		agreementNote.setUpdateUser(vo.getUpdateUser());
		if (vo.getUpdateUser() != null) {
			agreementNote.setUpdateUserFullName(personDao.getUserFullNameByUserName(vo.getUpdateUser()));
		}
		agreementNote.setAgreementRequestId(vo.getAgreementRequestId());
		agreementNote.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		addAgreementAttachment(agreementNote, files);
		agreementDao.saveOrUpdateAgreementNotes(agreementNote);
	}

	private void addLocationComment(NegotiationsLocation negotiationsLocation, Integer negotiationId, String comment, String updateUser, MultipartFile[] files) {
		NegotiationsComment negotiationComment = new NegotiationsComment();
		Integer locationId = negotiationsLocation.getNegotiationLocationId();
		negotiationComment.setComments(comment);
		negotiationComment.setNegotiationId(negotiationId);
		negotiationComment.setNegotiationLocationId(locationId);
		negotiationComment.setNegotiationsLocation(negotiationsLocation);
		negotiationComment.setCommentTypeCode("4");
		negotiationComment.setUpdateUser(updateUser);
		negotiationComment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
		addLocationAttachment(negotiationComment, files);
		negotiationAgreementDao.saveOrUpdateNegotiationsComment(negotiationComment);
	}*/

	/*private void addLocationAttachment(NegotiationsComment negotiationComment, MultipartFile[] files) {
		List<NegotiationCommentAttachment> negotiationCommentAttachments = new ArrayList<>();
		try {
			if (files != null && files.length > 0) {
				for (int i = 0; i < files.length; i++) {
					File file = new File(files[i].getOriginalFilename());
					String fileName = file.getName();
					NegotiationCommentAttachment negotiationCommentAttachment = new NegotiationCommentAttachment();
					negotiationCommentAttachment.setNegotiationsComment(negotiationComment);
					negotiationCommentAttachment.setFileName(fileName);
					negotiationCommentAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					negotiationCommentAttachment.setUpdateUser(negotiationComment.getUpdateUser());
					negotiationCommentAttachment.setMimeType(files[i].getContentType());
					NegotiationCommentFileData negotiationCommentFileData = new NegotiationCommentFileData();
					negotiationCommentFileData.setFileData(files[i].getBytes());
					negotiationCommentFileData = commonDao.saveFileData(negotiationCommentFileData);
					negotiationCommentAttachment.setNegotiationAttachmentFileId(negotiationCommentFileData.getNegotiationCommentFileDataId());
					negotiationCommentAttachments.add(negotiationCommentAttachment);
				}
				negotiationComment.getNegotiationCommentAttachment().addAll(negotiationCommentAttachments);
			}
		} catch (Exception e) {
			logger.info("Error while adding LocationAttachment");
		}
	}*/

	@Override
	public void addAgreementAttachment(AgreementNote agreementNote, MultipartFile[] files) {
		List<AgreementNoteAttachment> agreementNoteAttachments = new ArrayList<>();
		try {
			if (files != null && files.length > 0) {
				for (int i = 0; i < files.length; i++) {
					File file = new File(files[i].getOriginalFilename());
					String fileName = file.getName();
					AgreementNoteAttachment agreementNoteAttachment = new AgreementNoteAttachment();
					agreementNoteAttachment.setAgreementNote(agreementNote);
					agreementNoteAttachment.setFileName(fileName);
					agreementNoteAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
					agreementNoteAttachment.setUpdateUser(agreementNote.getUpdateUser());
					agreementNoteAttachment.setMimeType(files[i].getContentType());
					AgreementNoteFileData agreementNoteFileData = new AgreementNoteFileData();
					agreementNoteFileData.setFileData(files[i].getBytes());
					agreementNoteFileData = commonDao.saveFileData(agreementNoteFileData);
					agreementNoteAttachment.setAgreementNoteFileId(agreementNoteFileData.getAgreementNoteFileDataId());
					agreementNoteAttachments.add(agreementNoteAttachment);
				}
				agreementNote.getAgreementNoteAttachment().addAll(agreementNoteAttachments);
			}
		} catch (Exception e) {
			logger.info("Error while adding AgreementAttachment");
		}
	}

	/*@Override
	public String addLocationReviewComment(MultipartFile[] files, String formDataJSON) {
		ObjectMapper mapper = new ObjectMapper();
		AgreementVO vo = null;
		try {
		vo = mapper.readValue(formDataJSON, AgreementVO.class);
		NegotiationsComment negotiationsComment = vo.getNegotiationsComment();
		negotiationsComment.setCommentTypeCode(Constants.NEGOTIATION_COMMENT_LOCATION);
		negotiationsComment.setNegotiationsCommentType(negotiationAgreementDao.fetchNegotiationsCommentTypeById(Constants.NEGOTIATION_COMMENT_LOCATION));
		addLocationAttachment(negotiationsComment, files);
		vo.setNegotiationsComment(negotiationAgreementDao.saveOrUpdateNegotiationsComment(negotiationsComment));
		} catch (Exception e) {
			logger.info("Error while adding LocationReviewComment");
		}
		return commonDao.convertObjectToJSON(vo);
	}*/

	/*@Override
	public String loadReviewComments(AgreementVO vo) {
		vo.setNegotiationsComments(prepareLocationComments(negotiationAgreementDao.fetchNegotiationCommentBasedOnParams(vo.getNegotiationLocationId())));
		return commonDao.convertObjectToJSON(vo);
	}*/

	/*@Override
	public List<AgreementComment> prepareAgreementComment(Integer agreementRequestId) {
		List<AgreementComment> agreementComments = new ArrayList<>();
		Integer negotiationId = agreementService.getNegotiationIdBasedOnAgreementId(agreementRequestId);
		List<AgreementNote> agreementNotes = agreementDao.fetchAgreementNoteBasedOnAgreementId(agreementRequestId);
		List<NegotiationsComment> negotiationsComments = negotiationAgreementDao.fetchNegotiationCommentByNegotiationId(negotiationId);
		if (agreementNotes != null && !agreementNotes.isEmpty()) {
			for (AgreementNote agreementNote : agreementNotes) {
				if (agreementNote.getActionLogId() == null) {
					List<Map<Integer, String>> attachmentDetails = new ArrayList<>();
					AgreementComment agreementComment = new AgreementComment();
					agreementComment.setCommentId(agreementNote.getAgreementNoteId());
					agreementComment.setComment(agreementNote.getNote());
					agreementComment.setCommentType("G");
					agreementComment.setUpdateTimestamp(agreementNote.getUpdateTimestamp());
					agreementComment.setUpdateUser(agreementNote.getUpdateUser());
					if (agreementNote.getUpdateUser() != null) {
						agreementComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementNote.getUpdateUser()));
					}
					if (agreementNote.getAgreementNoteAttachment() != null&& !agreementNote.getAgreementNoteAttachment().isEmpty()) {
						List<AgreementNoteAttachment> agreementNoteAttachments = agreementNote.getAgreementNoteAttachment();
						for (AgreementNoteAttachment agreementNoteAttachment : agreementNoteAttachments) {
							Map<Integer, String> attachmentDetail = new HashMap<Integer, String>();
							attachmentDetail.put(agreementNoteAttachment.getAgreementNoteAttachmentId(),agreementNoteAttachment.getFileName());
							attachmentDetails.add(attachmentDetail);
						}
						agreementComment.setAttachmentDetails(attachmentDetails);
					}
					agreementComments.add(agreementComment);
				}
			}
		}
		if (negotiationsComments != null && !negotiationsComments.isEmpty()) {
			for (NegotiationsComment negotiationsComment :negotiationsComments) {
				List<Map<Integer, String>> attachmentDetails = new ArrayList<Map<Integer,String>>();
				AgreementComment agreementComment = new AgreementComment();
				agreementComment.setCommentId(negotiationsComment.getNegotiationCommentId());
				agreementComment.setComment(negotiationsComment.getComments());
				agreementComment.setCommentType("L");
				if (negotiationsComment.getNegotiationLocationId() != null) {
					agreementComment.setNegotiationLocationId(negotiationsComment.getNegotiationLocationId());
				}
				agreementComment.setUpdateTimestamp(negotiationsComment.getUpdateTimestamp());
				agreementComment.setUpdateUser(negotiationsComment.getUpdateUser());
				if (negotiationsComment.getUpdateUser() != null) {
					agreementComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsComment.getUpdateUser()));
				}
				if (negotiationsComment.getNegotiationsLocation() != null && negotiationsComment.getNegotiationsLocation().getLocationTypeCode() != null) {
					agreementComment.setLocationName(negotiationsComment.getNegotiationsLocation().getNegotiationsLocationType().getDescription());
				}
				if (negotiationsComment.getNegotiationCommentAttachment() != null && !negotiationsComment.getNegotiationCommentAttachment().isEmpty()) {
					List <NegotiationCommentAttachment> negotiationCommentAttachments = negotiationsComment.getNegotiationCommentAttachment();
					for (NegotiationCommentAttachment negotiationCommentAttachment : negotiationCommentAttachments) {
						Map <Integer, String> attachmentDetail = new HashMap<Integer, String>();
						attachmentDetail.put(negotiationCommentAttachment.getNegotiationCommentAttachmentId(), negotiationCommentAttachment.getFileName());
						attachmentDetails.add(attachmentDetail);
					}
					agreementComment.setAttachmentDetails(attachmentDetails);
				}
				agreementComments.add(agreementComment);
			}
		}
		Collections.sort(agreementComments, (agreementCommentOne, agreementCommentTwo) -> agreementCommentOne.getUpdateTimestamp().after(agreementCommentTwo.getUpdateTimestamp()) ? -1
				: agreementCommentOne.getUpdateTimestamp() == agreementCommentTwo.getUpdateTimestamp()? 0 : 1);
		return agreementComments;
	}*/

	/*@Override
	public List<NegotiationsComment> prepareLocationComments(List<NegotiationsComment> negotiationsComments) {
		if (negotiationsComments != null && !negotiationsComments.isEmpty()) {
			for (NegotiationsComment negotiationsComment : negotiationsComments) {
				if (negotiationsComment.getUpdateUser() != null) {
					negotiationsComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsComment.getUpdateUser()));
				}
			}
		}
		return negotiationsComments;
	}*/

	/*@Override
	public String deleteCommentAttachment(AgreementVO vo) {
		try {
			if (Boolean.TRUE.equals(vo.getIsLocationComment())) {
				NegotiationCommentAttachment negotiationCommentAttachment = negotiationAgreementDao.fetchNegotiationCommentAttachmentById(vo.getAttachmentId());
				if (negotiationCommentAttachment != null) {
					commonDao.deleteFileData(commonDao.getNegotiationCommentFileDataById(negotiationCommentAttachment.getNegotiationAttachmentFileId()));
					negotiationAgreementDao.deleteNegotiationCommentAttachment(negotiationCommentAttachment);
				}
			} else {
				AgreementNoteAttachment agreementmentNoteAttachment = agreementDao.fetchAgreementNoteAttachmentById(vo.getAttachmentId());
				if (agreementmentNoteAttachment != null) {
					commonDao.deleteFileData(commonDao.getAgreementNoteFileDataById(agreementmentNoteAttachment.getAgreementNoteFileId()));
					agreementDao.deleteAgreementNoteAttachment(agreementmentNoteAttachment);
				}
			}
			vo.setMessage("Deleted Sucessfully");
			vo.setAgreementComments(prepareAgreementCommentForDelete(vo));
			return commonDao.convertObjectToJSON(vo);
		} catch (Exception e) {
			e.printStackTrace();
			vo.setMessage("Exception Occured while deleting Comment Attachment");
			return commonDao.convertObjectToJSON(vo);
		}
	}*/

	/*private List<AgreementComment> prepareAgreementCommentForDelete(AgreementVO vo) {
		Integer agreementRequestId = vo.getAgreementRequestId();
		Integer attachmentId = vo.getAttachmentId();
		List<AgreementComment> agreementComments = new ArrayList<>();
		Integer negotiationId = agreementService.getNegotiationIdBasedOnAgreementId(agreementRequestId);
		List<AgreementNote> agreementNotes = agreementDao.fetchAgreementNoteBasedOnAgreementId(agreementRequestId);
		List<NegotiationsComment> negotiationsComments = negotiationAgreementDao.fetchNegotiationCommentByNegotiationId(negotiationId);
		if (agreementNotes != null && !agreementNotes.isEmpty()) {
			for (AgreementNote agreementNote : agreementNotes) {
				List<Map<Integer, String>> attachmentDetails = new ArrayList<Map<Integer,String>>();
				AgreementComment agreementComment = new AgreementComment();
				agreementComment.setCommentId(agreementNote.getAgreementNoteId());
				agreementComment.setComment(agreementNote.getNote());
				agreementComment.setCommentType("G");
				agreementComment.setUpdateTimestamp(agreementNote.getUpdateTimestamp());
				agreementComment.setUpdateUser(agreementNote.getUpdateUser());
				if (agreementNote.getUpdateUser() != null) {
					agreementComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementNote.getUpdateUser()));
				}
				if (agreementNote.getAgreementNoteAttachment() != null && !agreementNote.getAgreementNoteAttachment().isEmpty()) {
					List <AgreementNoteAttachment> agreementNoteAttachments = agreementNote.getAgreementNoteAttachment();
					List <AgreementNoteAttachment> copiedAgreementNoteAttachment = new ArrayList<AgreementNoteAttachment>(agreementNoteAttachments);
					Collections.copy(copiedAgreementNoteAttachment, agreementNoteAttachments);
					if (Boolean.FALSE.equals(vo.getIsLocationComment())) {
						for (AgreementNoteAttachment agreementNoteAttachment : copiedAgreementNoteAttachment) {
							if (attachmentId.equals(agreementNoteAttachment.getAgreementNoteAttachmentId())) {
								agreementNoteAttachments.remove(agreementNoteAttachment);
							}
						}
					}
					for (AgreementNoteAttachment agreementNoteAttachment : agreementNoteAttachments) {
						Map <Integer, String> attachmentDetail = new HashMap<Integer, String>();
						attachmentDetail.put(agreementNoteAttachment.getAgreementNoteAttachmentId(), agreementNoteAttachment.getFileName());
						attachmentDetails.add(attachmentDetail);
					}
					agreementComment.setAttachmentDetails(attachmentDetails);
				}
				agreementComments.add(agreementComment);
			}
		}
		if (negotiationsComments != null && !negotiationsComments.isEmpty()) {
			for (NegotiationsComment negotiationsComment :negotiationsComments) {
				List<Map<Integer, String>> attachmentDetails = new ArrayList<Map<Integer,String>>();
				AgreementComment agreementComment = new AgreementComment();
				agreementComment.setCommentId(negotiationsComment.getNegotiationCommentId());
				agreementComment.setComment(negotiationsComment.getComments());
				agreementComment.setCommentType("L");
				agreementComment.setUpdateTimestamp(negotiationsComment.getUpdateTimestamp());
				agreementComment.setUpdateUser(negotiationsComment.getUpdateUser());
				if (negotiationsComment.getUpdateUser() != null) {
					agreementComment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsComment.getUpdateUser()));
				}
				if (negotiationsComment.getNegotiationsLocation() != null && negotiationsComment.getNegotiationsLocation().getLocationTypeCode() != null) {
					agreementComment.setLocationName(negotiationsComment.getNegotiationsLocation().getNegotiationsLocationType().getDescription());
				}
				if (negotiationsComment.getNegotiationCommentAttachment() != null && !negotiationsComment.getNegotiationCommentAttachment().isEmpty()) {
					List <NegotiationCommentAttachment> negotiationCommentAttachments = negotiationsComment.getNegotiationCommentAttachment();
					List <NegotiationCommentAttachment> copiedNegotiationCommentAttachment = new ArrayList<NegotiationCommentAttachment>(negotiationCommentAttachments);
					Collections.copy(copiedNegotiationCommentAttachment, negotiationCommentAttachments);
					if (Boolean.TRUE.equals(vo.getIsLocationComment())) {
						for (NegotiationCommentAttachment negotiationCommentAttachment : copiedNegotiationCommentAttachment) {
							if (attachmentId.equals(negotiationCommentAttachment.getNegotiationCommentAttachmentId())) {
								negotiationCommentAttachments.remove(negotiationCommentAttachment);
							}
						}
					}
					for (NegotiationCommentAttachment negotiationCommentAttachment : negotiationCommentAttachments) {
						Map <Integer, String> attachmentDetail = new HashMap<Integer, String>();
						attachmentDetail.put(negotiationCommentAttachment.getNegotiationCommentAttachmentId(), negotiationCommentAttachment.getFileName());
						attachmentDetails.add(attachmentDetail);
					}
					agreementComment.setAttachmentDetails(attachmentDetails);
				}
				agreementComments.add(agreementComment);
			}
		}
		Collections.sort(agreementComments, (agreementCommentOne, agreementCommentTwo) -> agreementCommentOne.getUpdateTimestamp().after(agreementCommentTwo.getUpdateTimestamp()) ? -1
				: agreementCommentOne.getUpdateTimestamp() == agreementCommentTwo.getUpdateTimestamp()? 0 : 1);
		return agreementComments;
	}*/

	@Override
	public ResponseEntity<byte[]> downloadCommentAttachment(AgreementVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			if (Boolean.TRUE.equals(vo.getIsLocationComment())) {
				NegotiationCommentAttachment negotiationCommentAttachment = negotiationAgreementDao.fetchNegotiationCommentAttachmentById(vo.getAttachmentId());
				NegotiationCommentFileData negotiationCommentFileData = commonDao.getNegotiationCommentFileDataById(negotiationCommentAttachment.getNegotiationAttachmentFileId());
				byte[] data = negotiationCommentFileData.getFileData();
				HttpHeaders headers = new HttpHeaders();
				headers.setContentType(MediaType.parseMediaType(negotiationCommentAttachment.getMimeType()));
				String filename = negotiationCommentAttachment.getFileName();
				headers.setContentDispositionFormData(filename, filename);
				headers.setContentLength(data.length);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
			} else {
				AgreementNoteAttachment agreementNoteAttachment = agreementDao.fetchAgreementNoteAttachmentById(vo.getAttachmentId());
				AgreementNoteFileData agreementNoteFileData = commonDao.getAgreementNoteFileDataById(agreementNoteAttachment.getAgreementNoteFileId());
				byte[] data = agreementNoteFileData.getFileData();
				HttpHeaders headers = new HttpHeaders();
				headers.setContentType(MediaType.parseMediaType(agreementNoteAttachment.getMimeType()));
				String filename = agreementNoteAttachment.getFileName();
				headers.setContentDispositionFormData(filename, filename);
				headers.setContentLength(data.length);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
			}
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in download AgreementCommentAttachment: {} ", e.getMessage());
		}
		return attachmentData;
	}

}
