package com.polus.fibicomp.correspondence.service;

import java.nio.charset.StandardCharsets;

import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import com.polus.fibicomp.correspondence.dao.CorrespondenceDao;
import com.polus.fibicomp.correspondence.dto.CorrespondenceDataBus;
import com.polus.fibicomp.correspondence.dto.IRBCorrespondenceDto;

@Service(value = "correspondenceService")
public class CorrespondenceServiceImpl implements CorrespondenceService {
	protected static Logger logger = LogManager.getLogger(CorrespondenceServiceImpl.class.getName());
	@Autowired 
	private CorrespondenceDao correspondenceDao;

	@Override
	public ResponseEntity<byte[]> generateCorrespondence(HttpServletResponse response,
			CorrespondenceDataBus correspondenceDataBus) {
		ResponseEntity<byte[]> attachmentData = null;
		try{
			byte[] data = correspondenceDao.getTemplateData(correspondenceDataBus);	
			IRBCorrespondenceDto irbCorrespondenceDto = correspondenceDao.fetchIRBCorrespondenceData(correspondenceDataBus);
			byte[] mergedOutput = correspondenceDao.mergePlaceHolders(correspondenceDataBus.getOutputDataFormat(), data,irbCorrespondenceDto);
			String generatedFileName = "Result"+System.nanoTime()+".pdf";
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType("application/pdf"));
			headers.setContentDispositionFormData(generatedFileName, generatedFileName);
			headers.setContentLength(mergedOutput.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<byte[]>(mergedOutput, headers, HttpStatus.OK);
			response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			response.setContentType("application/pdf");
			response.setContentLength(mergedOutput.length);
			response.setHeader("Content-Disposition", "attachment; filename=\""+generatedFileName+"\"");
			FileCopyUtils.copy(mergedOutput, response.getOutputStream());			
		}catch (Exception e) {
			logger.error("Exception in generateCorrespondence"+ e.getMessage());
		}
		return attachmentData;
	}
}
