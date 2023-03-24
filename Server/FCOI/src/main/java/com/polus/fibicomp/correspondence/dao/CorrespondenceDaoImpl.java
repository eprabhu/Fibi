package com.polus.fibicomp.correspondence.dao;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.correspondence.dto.CorrespondenceDataBus;
import com.polus.fibicomp.correspondence.dto.IRBCorrespondenceDto;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.utils.QueryBuilder;

import fr.opensagres.xdocreport.document.IXDocReport;
import fr.opensagres.xdocreport.document.registry.XDocReportRegistry;
import fr.opensagres.xdocreport.template.IContext;
import fr.opensagres.xdocreport.template.TemplateEngineKind;

@Service(value = "correspondenceDao")
public class CorrespondenceDaoImpl implements CorrespondenceDao{
	protected static Logger logger = LogManager.getLogger(CorrespondenceDaoImpl.class.getName());
	private DBEngine dbEngine;
	
	public CorrespondenceDaoImpl() {
		dbEngine = new DBEngine();
	}

	public byte[] getTemplateData(CorrespondenceDataBus correspondenceDataBus) {
		byte[] data =null;
		String templateTypeCode =null;
		try{
			switch (correspondenceDataBus.getModuleCode()) {
			case 7:
				templateTypeCode = getLetterTemplateTypeCode(correspondenceDataBus);
				break;
			}
			data = getLetterTemplate(templateTypeCode);
		}catch (Exception e) {
			logger.error("Exception in getTemplateData"+ e.getMessage());
		}
		return data;
	}

	private byte[] getLetterTemplate(String templateTypeCode) {
		byte[] data =null;
		try{
			String query = QueryBuilder.selectLetterTemplate(templateTypeCode);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
			if(!dataList.isEmpty()){
				ByteArrayOutputStream byteArrayOutputStream = null;
				byteArrayOutputStream = (ByteArrayOutputStream) dataList.get(0).get("CORRESPONDENCE_TEMPLATE");
				data = byteArrayOutputStream.toByteArray();				
			}
		}catch (Exception e) {
			logger.error("Exception in getLetterTemplate"+ e.getMessage());
		}
		return data;
	}

	private String getLetterTemplateTypeCode(CorrespondenceDataBus correspondenceDataBus) {
		String templateTypeCode = null;
		try{
			String query = QueryBuilder.selectLetterTypeCode(correspondenceDataBus);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
			if(!dataList.isEmpty()){
				templateTypeCode = (dataList.get(0).get("LETTER_TEMPLATE_TYPE_CODE").toString());
			}		
		}catch (Exception e) {
			logger.error("Exception in getLetterTemplateTypeCode"+ e.getMessage());
		}
		return templateTypeCode;
	}

	public IRBCorrespondenceDto fetchIRBCorrespondenceData(CorrespondenceDataBus correspondenceDataBus) {
		IRBCorrespondenceDto irbCorrespondenceDto = new IRBCorrespondenceDto();
		try{
			irbCorrespondenceDto = getProtocolPrimaryFields(correspondenceDataBus,irbCorrespondenceDto);
			irbCorrespondenceDto = getPrincipleInvestigator(correspondenceDataBus,irbCorrespondenceDto);
			irbCorrespondenceDto = getProtocolActionDate(correspondenceDataBus,irbCorrespondenceDto);
		}catch (Exception e) {
			logger.error("Exception in fetchIRBCorrespondenceData"+ e.getMessage());
		}
		return irbCorrespondenceDto;
	}

	private IRBCorrespondenceDto getProtocolActionDate(CorrespondenceDataBus correspondenceDataBus,
			IRBCorrespondenceDto irbCorrespondenceDto) {
		try{
			String query = QueryBuilder.selectQueryForActionDate(correspondenceDataBus);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
			irbCorrespondenceDto.setActionDate(dataList.get(0).get("ACTION_DATE").toString());
		}catch (Exception e) {
			logger.error("Exception in getProtocolActionDate"+ e.getMessage());
		}
		return irbCorrespondenceDto;
	}

	private IRBCorrespondenceDto getPrincipleInvestigator(CorrespondenceDataBus correspondenceDataBus,
			IRBCorrespondenceDto irbCorrespondenceDto) {
		try{
			String query = QueryBuilder.selectQueryForPIName(correspondenceDataBus);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
			irbCorrespondenceDto.setToUserName(dataList.get(0).get("FULL_NAME").toString());
		}catch (Exception e) {
			logger.error("Exception in getPrincipleInvestigator"+ e.getMessage());
		}
		return irbCorrespondenceDto;
	}

	private IRBCorrespondenceDto getProtocolPrimaryFields(CorrespondenceDataBus correspondenceDataBus,IRBCorrespondenceDto irbCorrespondenceDto) throws Exception {
		String query = QueryBuilder.selectQueryForProtocolDetails(correspondenceDataBus);
		ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),query);
		irbCorrespondenceDto.setCommitteAction(correspondenceDataBus.getCommitteAction());
		irbCorrespondenceDto.setStudyTitle(dataList.get(0).get("TITLE").toString());
		irbCorrespondenceDto.setExpirationDate(dataList.get(0).get("EXPIRATION_DATE").toString());
		irbCorrespondenceDto.setProtocolNumber(correspondenceDataBus.getModuleItemKey());
		return irbCorrespondenceDto;
	}

	public byte[] mergePlaceHolders(String outputDataFormat,byte[] data, IRBCorrespondenceDto irbCorrespondenceDto) {
		byte[] mergedOutput = null;
		try{
			InputStream myInputStream = new ByteArrayInputStream(data); 
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,TemplateEngineKind.Velocity);
			//FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			IContext context = report.createContext();
			context = setPlaceHolderData(context,irbCorrespondenceDto);
			DocxDocumentMergerAndConverter docxDocumentMergerAndConverter = new DocxDocumentMergerAndConverter();
			if(outputDataFormat.equalsIgnoreCase("pdf")){
//				mergedOutput = docxDocumentMergerAndConverter.mergeAndGeneratePDFOutput(myInputStream,report,null, TemplateEngineKind.Velocity,context);
			}else{
//				mergedOutput =docxDocumentMergerAndConverter.generateMergedOutput(report, context);
			}
		}catch (Exception e) {
			logger.error("Exception in mergePlaceHolders"+ e.getMessage());
		}
		return mergedOutput;
	}

	private IContext setPlaceHolderData(IContext context, IRBCorrespondenceDto irbCorrespondenceDto) {
		context.put("TO", irbCorrespondenceDto.getToUserName() == null ? "": irbCorrespondenceDto.getToUserName());
		context.put("COMMITTEE_ACTION", irbCorrespondenceDto.getCommitteAction() == null ? "": irbCorrespondenceDto.getCommitteAction());
		context.put("PROTOCOL_NUMBER", irbCorrespondenceDto.getProtocolNumber() == null ? "": irbCorrespondenceDto.getProtocolNumber());
		context.put("TITLE", irbCorrespondenceDto.getStudyTitle() == null ? "": irbCorrespondenceDto.getStudyTitle());
		context.put("EXPIRATION_DATE", irbCorrespondenceDto.getExpirationDate() == null ? "": irbCorrespondenceDto.getExpirationDate());	
		context.put("EXPIRATION_DATE", irbCorrespondenceDto.getExpirationDate() == null ? "": irbCorrespondenceDto.getExpirationDate());
		context.put("ACTION_DATE", irbCorrespondenceDto.getActionDate() == null ? "": irbCorrespondenceDto.getActionDate());
		return context;
	}



}
