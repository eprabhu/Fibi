package com.polus.fibicomp.coi.service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import com.polus.core.applicationexception.dto.ApplicationException;
import com.polus.core.common.service.CommonService;
import com.polus.core.filemanagement.FileManagementOutputDto;
import com.polus.core.filemanagement.FileManagementService;
import com.polus.core.filemanagement.FileManagmentConstant;
import com.polus.core.filemanagement.FileManagmentInputDto;
import com.polus.fibicomp.constants.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.COIFileAttachmentDao;
import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.exception.COIFileAttachmentException;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.core.security.AuthenticatedUser;
import com.polus.fibicomp.coi.dto.PersonAttachmentDto;
import com.polus.fibicomp.coi.pojo.Attachments;

@Transactional
@Service
public class COIFileAttachmentServiceImpl implements COIFileAttachmentService {

	@Autowired
	FileManagementService fileManagementService;

	@Autowired
	COIFileAttachmentDao coiFileAttachmentDao;

	@Autowired
	private CommonService commonService;

	private static final String COI_MODULE_CODE = "8";

    public COIFileAttachmentServiceImpl(FileManagementService fileManagementService) {
        this.fileManagementService = fileManagementService;
    }

	@Override
	@Transactional(rollbackFor = {COIFileAttachmentException.class, IOException.class})
	public String saveFileAttachment(COIFileRequestDto request) {
		FileManagementOutputDto fileOutput = null;
		try {
			FileManagmentInputDto input = FileManagmentInputDto.builder()
											.file(request.getFile())
											.moduleCode(FileManagmentConstant.COI_MODULE_CODE)
											.moduleNumber(request.getComponentReferenceNumber())
											.updateUser(AuthenticatedUser.getLoginUserName())
											.build();
			fileOutput = fileManagementService.saveFile(input);
			request.setFileDataId(fileOutput.getFileDataId());
			coiFileAttachmentDao.saveDisclosureAttachmentDetail(request);
		} catch (Exception e) {
			fileManagementService.removeFileOnException(fileOutput.getFilePath(), fileOutput.getFileName());
			throw new COIFileAttachmentException("Exception in saveFileAttachment in COIFileAttachmentService, " + e);
		}
		return "SUCCESS";
	}

	@Override
	@Transactional(rollbackFor = {COIFileAttachmentException.class, IOException.class})
	public Attachments saveAttachment(PersonAttachmentDto request, String personId) {
		FileManagementOutputDto fileOutput = null;
		try {
			FileManagmentInputDto input = FileManagmentInputDto.builder()
											.file(request.getFile())
											.moduleCode(FileManagmentConstant.COI_MODULE_CODE)
											.moduleNumber(personId)
											.updateUser(AuthenticatedUser.getLoginUserName())
											.build();
			fileOutput = fileManagementService.saveFile(input);
			request.setFileDataId(fileOutput.getFileDataId());
			Attachments attachment= coiFileAttachmentDao.saveAttachmentDetails(request, request.getFileDataId());
			return attachment;
		} catch (Exception e) {
			fileManagementService.removeFileOnException(fileOutput.getFilePath(), fileOutput.getFileName());
			throw new COIFileAttachmentException("Exception in saveFileAttachment in COIFileAttachmentService, " + e);
		}
	}

	@Override
	public List<DisclAttachment> getDisclAttachByRefId(Integer refId) {
		List<DisclAttachment> disclAttachments = coiFileAttachmentDao.getDisclAttachByRefId(refId);
		return disclAttachments;
	}

	@Override
	public List<DisclAttachment> getDisclAttachByRefIdAndTypeCode(Integer refId, Integer typeCode) {
		List<DisclAttachment> disclAttachments = coiFileAttachmentDao.getDisclAttachByRefIdAndTypeCode(refId, typeCode);
		return disclAttachments;
	}

	@Override
	public String updateDisclAttachment(COIFileRequestDto request) {
		try {
			FileManagmentInputDto input = FileManagmentInputDto.builder()
											.file(request.getFile())
											.moduleCode(COI_MODULE_CODE)
											.moduleNumber(request.getComponentReferenceNumber())
											.updateUser(AuthenticatedUser.getLoginUserName())
											.build();
			FileManagementOutputDto fileOutput = fileManagementService.saveFile(input);
			request.setFileDataId(fileOutput.getFileDataId());
			DisclAttachment disclosureAttachment = DisclAttachment.builder()
													.attachmentNumber(request.getAttachmentNumber())
													.versionNumber(request.getVersionNumber() + 1)
													.attaStatusCode(request.getAttaStatusCode())
													.attaTypeCode(request.getAttaTypeCode())
													.commentId(request.getCommentId())
													.componentReferenceId(request.getComponentReferenceId())
													.componentReferenceNumber(request.getComponentReferenceNumber())
													.componentTypeCode(request.getComponentTypeCode())
													.fileDataId(request.getFileDataId())
													.fileName(request.getFile().getName())
													.mimeType(request.getFile().getContentType())
													.description(request.getDescription())
													.documentOwnerPersonId(request.getDocumentOwnerPersonId())
													.updateUser(null)
													.updateTimestamp(null)
													.build();
			coiFileAttachmentDao.updateDisclosureAttachmentDetail(disclosureAttachment);
			coiFileAttachmentDao.updateDisclosureAttachmentStatus("3", request.getAttachmentId());// Archive to be set
		} catch (Exception e) {
			throw new COIFileAttachmentException("Exception in saveFileAttachment in COIFileAttachmentService, " + e);
		}
		return "SUCCESS";
	}

	@Override
	public String deleteDisclAttachment(COIFileRequestDto request) {
		fileManagementService.deleteFile(COI_MODULE_CODE,request.getFileDataId());
		coiFileAttachmentDao.deleteDisclAttachment(request.getAttachmentId());
		return "SUCCESS";
	}

	@Override
	public ResponseEntity<byte[]> downloadDisclAttachment(Integer attachmentId) {
		DisclAttachment attachment = coiFileAttachmentDao.getDisclAttachByAttachId(attachmentId);
		ResponseEntity<byte[]> attachmentData = null;
		try {
			FileManagementOutputDto fileData = fileManagementService.downloadFile(COI_MODULE_CODE, attachment.getFileDataId());
			attachmentData = setAttachmentContent(fileData.getOriginalFileName(), fileData.getData());
		} catch (Exception e) {
			throw new COIFileAttachmentException("Exception in downloadDisclAttachment in COIFileAttachmentService, " + e);
		}
		return attachmentData;
	}

	private ResponseEntity<byte[]> setAttachmentContent(String fileName, byte[] data) {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
		headers.setContentDispositionFormData(fileName, fileName);
		headers.setContentLength(data.length);
		headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
		headers.setPragma("public");
		return new ResponseEntity<>(data, headers, HttpStatus.OK);
	}

	@Override
	public void exportAllDisclAttachments(COIFileRequestDto request, HttpServletResponse response) throws IOException {
		List<Integer> attachmentIds = request.getAttachmentIds();
		Integer disclosureId = request.getComponentReferenceId();
		if (attachmentIds != null && !attachmentIds.isEmpty()) {
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append("Disclosure_#")
					        .append(disclosureId)
					        .append("_attachments");
			String fileName = stringBuilder.toString();
			response.setContentType("application/zip");
			response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
			List<DisclAttachment> attachments = coiFileAttachmentDao.getDisclAttachByAttachIds(attachmentIds);
			ZipOutputStream zos = null;
			ByteArrayOutputStream baos = null;
			try {
				baos = new ByteArrayOutputStream();
				zos = new ZipOutputStream(baos);
				if (attachments != null && !attachments.isEmpty()) {
					Integer index = 0;
					for (DisclAttachment attachment : attachments) {
						FileManagementOutputDto fileData = fileManagementService.downloadFile(COI_MODULE_CODE, attachment.getFileDataId());
						index = commonService.addFilesToZipFolder(index, fileData.getOriginalFileName(), zos);
						zos.write(fileData.getData());
					}
				}
			} catch (Exception e) {
				throw new COIFileAttachmentException("Exception in downloadDisclAttachment in COIFileAttachmentService, " + e);
			} finally {
				zos.closeEntry();
				zos.flush();
				baos.flush();
				zos.close();
				baos.close();
				ServletOutputStream op = response.getOutputStream();
				op.write(baos.toByteArray());
				op.flush();
				op.close();
			}
		}
	}

	@Override
	public String updateDisclAttachmentDetails(COIFileRequestDto request) {
		try {
			DisclAttachment disclAttachment = coiFileAttachmentDao.getDisclAttachByAttachId(request.getAttachmentId());
			disclAttachment.setAttachmentNumber(request.getAttachmentNumber());
			disclAttachment.setVersionNumber(request.getVersionNumber());
			disclAttachment.setAttaStatusCode(request.getAttaStatusCode());
			disclAttachment.setAttaTypeCode(request.getAttaTypeCode());
			disclAttachment.setCommentId(request.getCommentId());
			disclAttachment.setComponentReferenceId(request.getComponentReferenceId());
			disclAttachment.setComponentReferenceNumber(request.getComponentReferenceNumber());
			disclAttachment.setComponentTypeCode(request.getComponentTypeCode());
			disclAttachment.setFileDataId(request.getFileDataId());
			disclAttachment.setDescription(request.getDescription());
			disclAttachment.setDocumentOwnerPersonId(request.getDocumentOwnerPersonId());
			coiFileAttachmentDao.updateDisclosureAttachmentDetail(disclAttachment);
		} catch (Exception e) {
			throw new COIFileAttachmentException("Exception in saveFileAttachment in COIFileAttachmentService, " + e);
		}
		return "SUCCESS";
	}

	@Override
	public List<DisclAttachment> getDisclAttachByCommentId(Integer commentId) {
		return coiFileAttachmentDao.getDisclAttachByCommentId(commentId);
	}

	@Override
	public Integer deleteDisclAttachmentById(Integer attachmentId) {
		DisclAttachment attachment = coiFileAttachmentDao.getDisclAttachByAttachId(attachmentId);
		if (attachment == null) {
			throw new ApplicationException("Attachment not fount with id : " + attachmentId, Constants.JAVA_ERROR);
		}
		fileManagementService.deleteFile(COI_MODULE_CODE,attachment.getFileDataId());
		coiFileAttachmentDao.deleteDisclAttachment(attachmentId);
		return attachment.getCommentId();
	}
}
