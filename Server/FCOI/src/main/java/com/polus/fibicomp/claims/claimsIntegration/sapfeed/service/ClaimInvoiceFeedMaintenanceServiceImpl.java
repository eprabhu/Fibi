package com.polus.fibicomp.claims.claimsIntegration.sapfeed.service;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao.ClaimInvoiceFeedMaintenanceDao;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.dao.ClaimsDao;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.dao.SapFeedMaintenanceDao;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.service.SapFeedMaintenanceService;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.render.AwardClaimRenderService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;		
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;

@Transactional
@Service(value = "claimInvoiceFeedMaintenance")
public class ClaimInvoiceFeedMaintenanceServiceImpl implements ClaimInvoiceFeedMaintenanceService {

    @Autowired
    private ClaimInvoiceFeedMaintenanceDao claimInvoiceFeedMaintenanceDao;  
    
    @Autowired
	private SapFeedMaintenanceDao sapFeedMaintenanceDao;

    @Autowired
    private CommonDao commonDao;
    
    @Autowired
    private ClaimsDao claimsDao;
    
    @Autowired
    private PersonDao personDao;
    
    @Autowired
    private EmailService emailService;
    
    @Autowired
    private AwardClaimRenderService awardClaimRenderService;

    @Autowired
    private SftpConfigurationService sftpConfigurationService;

    @Autowired
    private SapFeedMaintenanceService sapFeedMaintenanceService;

    @Autowired
	private PrintService printService;
   
	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private CommonService commonService;

    protected static Logger logger = LogManager.getLogger(ClaimInvoiceFeedMaintenanceServiceImpl.class.getName());
    private static final String ADVANCESEARCHTYPE = "A";
    private static final String SUCCESS = "SUCCESS";
    private static final String FAILED = "FAILED";

    @Override
    public String getClaimBatchDashboard(SapFeedMaintenanceVO maintenanceVO) {
        return commonDao.convertObjectToJSON(prepareInvoiceFeeds(maintenanceVO));
    }

    private SapFeedMaintenanceVO prepareInvoiceFeeds(SapFeedMaintenanceVO maintenanceVO) {
    	if(maintenanceVO.getTabName().equals(Constants.BATCH_DETAIL) && !maintenanceVO.getIsAdvanceSearch().equals(ADVANCESEARCHTYPE)) {
            maintenanceVO.setSapClaimFeedBatch(claimInvoiceFeedMaintenanceDao.getClaimLatestBatchDashboard());
            if(maintenanceVO.getSapClaimFeedBatch() != null) {
            	maintenanceVO.setBatchId(maintenanceVO.getSapClaimFeedBatch().getBatchId());
            	maintenanceVO.setClaimInvoiceFeedDtoList(claimInvoiceFeedMaintenanceDao.getSapClaimFeedDashBoard(maintenanceVO));
            }
        } else {
            if(maintenanceVO.getBatchId() != null) {
                maintenanceVO.setSapClaimFeedBatch(claimInvoiceFeedMaintenanceDao.getSapFeedClaimBatchById(maintenanceVO.getBatchId()));
                maintenanceVO.getSapClaimFeedBatch().setErrorCount(claimInvoiceFeedMaintenanceDao.getSapClaimBatchCount(maintenanceVO.getBatchId()));
            }
            maintenanceVO.setClaimInvoiceFeedDtoList(claimInvoiceFeedMaintenanceDao.getSapClaimFeedDashBoard(maintenanceVO));
        }
        maintenanceVO.setSapFeedUserActions(sapFeedMaintenanceDao.getSapFeedUserAction());
        return maintenanceVO;
    }

	@Override
	public String loadClaimInvoiceLog(Integer invoiceId, Integer batchId) {
		Map<String, Object> result = new HashMap<>();
		List<ClaimInvoiceLog> claimInvoiceLog = claimInvoiceFeedMaintenanceDao.loadClaimInvoiceLog(invoiceId, batchId);
		result.put("claimInvoiceLog", claimInvoiceLog);
		List<Integer> claimInvoiceLogIds = claimInvoiceLog.stream().map(ClaimInvoiceLog::getClaimInvoiceLogId).collect(Collectors.toList());
		result.put("sapClaimFeedResponseMessage", claimInvoiceFeedMaintenanceDao.getSapClaimErrorMsg(claimInvoiceLogIds));
		return commonDao.convertObjectToJSON(result);
	}

	@Override
	public String claimInvoiceNotifyPI(SapFeedMaintenanceVO vo) {
		try {
			List<ClaimInvoiceFeedDto> sapClaimFeeds = vo.getClaimInvoiceFeedDtoList();
			List<String> claimNumbers = new ArrayList<>();
			sapClaimFeeds.stream().forEach(sapClaimFeed -> {
				if(isPersonHasRight(sapClaimFeed.getClaimId(), Constants.MAINTAIN_INVOICE_FEED).equals(Boolean.TRUE)) {
					claimInvoiceFeedMaintenanceDao.updateSapClaimFeedUserAction(vo.getSapFeedUserAction().getUserActionCode(), vo.getUserComment(), sapClaimFeed.getFeedId());
					sendSapClaimFeedNotification(vo, sapClaimFeed, Constants.SAP_CLAIM_FEED_REVISION_NOTIFICATION_CODE, new HashSet<>());
				} else {
					claimNumbers.add(sapClaimFeed.getClaimNumber());
				}
			});
			vo.setMessage(SUCCESS);
			vo.setClaimNumbers(claimNumbers);
			getClaimBatchDashboard(vo);
			return commonDao.convertObjectToJSON(vo);
	    } catch (Exception e) {
            logger.error("Error in claimInvoiceNotifyPI {}", e.getMessage());
	    	vo.setMessage(FAILED);
			getClaimBatchDashboard(vo);
	    	return commonDao.convertObjectToJSON(vo);
	    }
	}
	
	private Boolean isPersonHasRight(Integer claimId, String rightName) {
		String leadUnitNumber = claimsDao.getClaim(claimId).getAward().getLeadUnitNumber();
		if (leadUnitNumber != null) {
			return personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), rightName, leadUnitNumber);
		} else {
			return Boolean.FALSE;
		}
	}
	
	@Override
	public String updateClaimInvoiceFeedStatus(SapFeedMaintenanceVO vo) {
		try {
			List<String> claimNumbers = new ArrayList<>();
			vo.getClaimInvoiceLogs().forEach(invoice -> {
				if(isPersonHasRight(invoice.getClaimId(), Constants.MAINTAIN_INVOICE_FEED).equals(Boolean.TRUE)) {
					claimInvoiceFeedMaintenanceDao.updateClaimInvoiceLog(invoice.getClaimInvoiceLogId(), invoice.getOutputDocumentNumber(), invoice.getFiscalYear());
					claimInvoiceFeedMaintenanceDao.updateSapClaimFeed(vo.getFeedId(), vo.getSapFeedUserAction().getUserActionCode(), vo.getUserComment());
					claimsDao.updateClaimDetailByParams(invoice.getClaimId(),Constants.CLAIM_STATUS_CODE_INVOICE_GENERATED, null, null, null);
				} else {
					claimNumbers.add(invoice.getClaimNumber());
				}
				
			});
			vo.setClaimNumbers(claimNumbers);
			vo.setMessage(SUCCESS);
			getClaimBatchDashboard(vo);
			return commonDao.convertObjectToJSON(vo);
		} catch (Exception e) {
            logger.error("Error in updateClaimInvoiceFeedStatus {}", e.getMessage());
			vo.setMessage(FAILED);
			getClaimBatchDashboard(vo);
	    	return commonDao.convertObjectToJSON(vo);
		}
	}

	@Override
	public String reinterfaceClaimSapFeed(SapFeedMaintenanceVO vo) {
		ClaimInvoiceFeedDto claimInvoiceFeedDto = new ClaimInvoiceFeedDto();
		List<String> claimNumbers = new ArrayList<>();
		List<SapClaimFeed> claimFeeds = claimInvoiceFeedMaintenanceDao.getClaimFeedByIds(vo.getClaimInvoiceFeedDtoList().stream().
				filter(invoice -> invoice != null && invoice.getFeedId() != null).map(ClaimInvoiceFeedDto::getFeedId).collect(Collectors.toList()));
		List<Integer> feedIds = new ArrayList<>();
		claimFeeds.forEach(feed -> {
			if(isPersonHasRight(feed.getClaimId(), Constants.MAINTAIN_INVOICE_FEED).equals(Boolean.TRUE)) {
				feed.setUserActionCode(vo.getSapFeedUserAction().getUserActionCode());
				SapClaimFeed newFeed = new SapClaimFeed();
				BeanUtils.copyProperties(feed, newFeed);
				newFeed.setFeedId(null);
				newFeed.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				newFeed.setUpdateUser(AuthenticatedUser.getLoginUserName());
				newFeed.setFeedStatus("P");
				claimsDao.saveOrUpdateSapClaimFeed(newFeed);
				feed.setUserComment(vo.getUserComment());
				claimsDao.saveOrUpdateSapClaimFeed(feed);
				feedIds.add(newFeed.getFeedId());
				claimInvoiceFeedDto.setClaimId(feed.getClaimId());
				claimInvoiceFeedDto.setBatchId(feed.getBatchId());
				claimInvoiceFeedDto.setInvoiceId(feed.getInvoiceId());
				sendSapClaimFeedNotification(vo, claimInvoiceFeedDto, Constants.SAP_CLAIM_FEED_RE_INTERFACE_NOTIFICATION_CODE, new HashSet<>());
			} else {
				claimNumbers.add(feed.getClaimNumber());
			}		
		});
		vo.setClaimNumbers(claimNumbers);
		vo.setMessage(SUCCESS);
    	return commonDao.convertObjectToJSON(vo);
	}
	
	private ClaimInvoiceFeedDto sendSapClaimFeedNotification(SapFeedMaintenanceVO vo, ClaimInvoiceFeedDto dto, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.CLAIM_MODULE_CODE);
		emailServiceVO.setModuleItemKey(dto.getClaimId().toString());
		emailServiceVO.setPlaceHolder(getSapFeedClaimPlaceholders(dto, vo));
		emailServiceVO.setSubModuleCode(Constants.CLAIM_SUBMODULE_CODE.toString());
		if (dynamicEmailRecipients != null && !dynamicEmailRecipients.isEmpty()) {
			emailServiceVO.setRecipients(dynamicEmailRecipients);
		}
		emailService.sendEmail(emailServiceVO);
		return dto;
	}
	
	private Map<String, String> getSapFeedClaimPlaceholders(ClaimInvoiceFeedDto dto, SapFeedMaintenanceVO vo) {			
		Map<String, String> placeHolder = new HashMap<>();
		if (dto.getInvoiceId() != null && dto.getBatchId() != null) {
			List<Integer> claimInvoiceLogIds = claimInvoiceFeedMaintenanceDao.loadClaimInvoiceLog(dto.getInvoiceId(), dto.getBatchId()).stream().map(ClaimInvoiceLog::getClaimInvoiceLogId).collect(Collectors.toList());
			if(!claimInvoiceLogIds.isEmpty()) {
				List<SapClaimFeedResponseMessage> sapClaimFeedResponseMessage = claimInvoiceFeedMaintenanceDao.getSapClaimErrorMsg(claimInvoiceLogIds);
				placeHolder.put("{SAP_MESSAGE}", String.join(",", sapClaimFeedResponseMessage.stream().map(SapClaimFeedResponseMessage::getMessage).collect(Collectors.toList())));
			}
		}
		placeHolder.put("{USER_COMMENT}", vo.getUserComment());
		placeHolder.put("{USER_ACTION}", vo.getUserAction());
		Map<String, String> awardPlaceHolder = awardClaimRenderService.getPlaceHolderData(dto.getClaimId().toString());
		placeHolder.putAll(awardPlaceHolder);
		return placeHolder;
	}

	@Override
	public ResponseEntity<byte[]> exportClaimInvoiceAttachments(SapFeedMaintenanceVO vo) {
		StringBuilder fileName = new StringBuilder();
		Integer batchId = vo.getBatchId();
		FileInputStream claimFileStream = null;
		ResponseEntity<byte[]> attachmentData = null;
		fileName.append(batchId.toString()).append("_").append("attachments");
		try {
			String filepath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_CLAIM_INVOICE_INBOUND);
			File[] attachments = sapFeedMaintenanceService.getSapAwardFeedFiles(batchId, filepath);
			if (attachments != null && attachments.length != 0) {
				claimFileStream = new FileInputStream(attachments[0]);
			    byte[] fileData = new byte[(int)attachments[0].length()];
			    claimFileStream.read(fileData);
			    claimFileStream.close();
			    if (fileData.length > 0) {
			    	 attachmentData = printService.setAttachmentContent(fileName.toString(), fileData);
			    }
			}
		} catch (Exception e) {
			logger.error("Exception in downloadClaimAttachment {}", e.getMessage());
		} finally {
			try {
				if(claimFileStream != null) {
					claimFileStream.close();
				}
			} catch (IOException e) {
				logger.error("Exception in downloadClaimAttachment {}", e.getMessage());
			}
		}
		return attachmentData;
	}

	@Override
	public ResponseEntity<byte[]> generateInvoiceFeedReport(HttpServletResponse response, SapFeedMaintenanceVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			vo.setIsDownload(Boolean.TRUE);
			vo = prepareInvoiceFeeds(vo);
			attachmentData = generateInvoiceFeedExcelReport(vo.getClaimInvoiceFeedDtoList(), attachmentData, vo);
		} catch (Exception e) {
			logger.error("Exception in generateInvoiceFeedReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private ResponseEntity<byte[]> generateInvoiceFeedExcelReport(List<ClaimInvoiceFeedDto> claimInvoiceFeeds,
			ResponseEntity<byte[]> attachmentData, SapFeedMaintenanceVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		try {
			CommonVO commonVo = new CommonVO();
			commonVo.setExportType("xlsx");
			commonVo.setDocumentHeading(vo.getDocumentHeading());   
			XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
	        commonService.addDetailsInHeader(workbook, sheet);
	        prepareExcelSheetForInvoiceFeedReport(claimInvoiceFeeds, sheet, workbook, vo.getTabName(), vo.getDocumentHeading());
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = dashboardService.getResponseEntityForDownload(commonVo, workbook);
			workbook.close();
	        } catch (Exception e) {
				logger.error("error in generateInvoiceFeedExcelReport {}", e.getMessage());
			} finally {
				try {
					workbook.close();
				} catch (IOException e) {
					logger.error("error in generateInvoiceFeedExcelReport {}", e.getMessage());
				}
			}
			return attachmentData;
	}

	private void prepareExcelSheetForInvoiceFeedReport(List<ClaimInvoiceFeedDto> claimInvoiceFeeds, XSSFSheet sheet,
			XSSFWorkbook workbook, String tabName, String documentHeading) {
		try {
			XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
	        sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 4));
			int rowNumber = 0;
			int totalColumnCount = 7;
			rowNumber = rowNumber + 1;
			printService.prepareExcelSheetHeading(sheet, documentHeading, workbook, totalColumnCount, rowNumber);
			rowNumber = rowNumber + 1;
	        if (tabName.equals("BATCH_DETAIL")) {
	                Object[] tableHeadingYear = {"Feed ID", "Batch ID", "Claim Number", "Award Number", "Account Number", "Type", "Updated By", "Updated On", "Feed Status", "Follow Up", "Comment"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			} else if (tabName.equals("PENDING_FEEDS")) {
	                Object[] tableHeadingYear = {"Feed ID", "Claim Number", "Award Number", "Type", "Updated By", "Updated On", "Feed Status"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			} else if (tabName.equals("BATCH_HISTORY")) {
	                Object[] tableHeadingYear = {"Batch ID", "Total Invoices", "Error Invoices", "Batch Generated on", "Response Processed on"};
	                printService.prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
			}
			prepareInvoiceFeedReport(claimInvoiceFeeds, rowNumber, tabName, sheet, tableBodyStyle);
	        } catch (Exception e) {
				logger.error("error in prepareExcelSheetForInvoiceFeedReport {}", e.getMessage());
			}
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	private void prepareInvoiceFeedReport(List<ClaimInvoiceFeedDto> claimInvoiceFeeds, int rowNumber, String tabName, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle) {
		if (tabName.equals("BATCH_DETAIL")) {
			prepareInvoiceBatchDetail(rowNumber, sheet, tableBodyStyle, claimInvoiceFeeds);
        } else if (tabName.equals("PENDING_FEEDS")) {
        	prepareInvoicePendingFeedDetail(rowNumber, sheet, tableBodyStyle, claimInvoiceFeeds);
		} else if (tabName.equals("BATCH_HISTORY")) {
			prepareInvoiceBatchHistoryDetail(rowNumber, sheet, tableBodyStyle, claimInvoiceFeeds);
		}
	}

	private void prepareInvoiceBatchHistoryDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<ClaimInvoiceFeedDto> claimInvoiceFeeds) {
		for (ClaimInvoiceFeedDto claimInvoiceFeed : claimInvoiceFeeds) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell batchIdCell = assignCell(cellNumber, tableBodyStyle, row);
			batchIdCell.setCellValue(claimInvoiceFeed.getBatchId() != null ? claimInvoiceFeed.getBatchId().toString() : "");
			cellNumber++;
	
			Cell noOfCountCell = assignCell(cellNumber, tableBodyStyle, row);
			noOfCountCell.setCellValue(claimInvoiceFeed.getCountOfRecords() != null ? claimInvoiceFeed.getCountOfRecords().toString() : "");
			cellNumber++;
	
			Cell errorCell = assignCell(cellNumber, tableBodyStyle, row);
			errorCell.setCellValue(claimInvoiceFeed.getErrorCount() != null && !claimInvoiceFeed.getErrorCount().toString().equals("0") ? claimInvoiceFeed.getErrorCount().toString() : "No Error");
			cellNumber++;
	
			Cell createdOnCell = assignCell(cellNumber, tableBodyStyle, row);
			createdOnCell.setCellValue(claimInvoiceFeed.getCreateTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(claimInvoiceFeed.getCreateTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(claimInvoiceFeed.getResponseTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(claimInvoiceFeed.getResponseTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
			rowNumber++;
		}
	}

	private void prepareInvoicePendingFeedDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<ClaimInvoiceFeedDto> claimInvoiceFeeds) {
		for (ClaimInvoiceFeedDto claimInvoiceFeed : claimInvoiceFeeds) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell feedIdCell = assignCell(cellNumber, tableBodyStyle, row);
			feedIdCell.setCellValue(claimInvoiceFeed.getFeedId() != null ? claimInvoiceFeed.getFeedId().toString() : "");
			cellNumber++;
	
			Cell claimNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			claimNumberCell.setCellValue(claimInvoiceFeed.getClaimNumber() != null ? claimInvoiceFeed.getClaimNumber() : "");
			cellNumber++;
	
			Cell awardNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			awardNumberCell.setCellValue(claimInvoiceFeed.getAwardNumber() != null ? claimInvoiceFeed.getAwardNumber() : "");
			cellNumber++;
	
			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(claimInvoiceFeed.getFeedType() != null ? claimInvoiceFeed.getFeedType() :"");
			cellNumber++;
	
			Cell updatedByCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedByCell.setCellValue(claimInvoiceFeed.getUpdateUser() != null ? claimInvoiceFeed.getUpdateUser() :"System");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(claimInvoiceFeed.getUpdateTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(claimInvoiceFeed.getUpdateTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell statusCell = assignCell(cellNumber, tableBodyStyle, row);
			statusCell.setCellValue(claimInvoiceFeed.getFeedStatus() != null ? claimInvoiceFeed.getFeedStatus() : "");
			cellNumber++;
			rowNumber++;
		}
	}

	private void prepareInvoiceBatchDetail(int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<ClaimInvoiceFeedDto> claimInvoiceFeeds) {
		for (ClaimInvoiceFeedDto claimInvoiceFeed : claimInvoiceFeeds) {
			int cellNumber = 0;
			Row row = sheet.createRow(rowNumber);
			Cell feedIdCell = assignCell(cellNumber, tableBodyStyle, row);
			feedIdCell.setCellValue(claimInvoiceFeed.getFeedId() != null ? claimInvoiceFeed.getFeedId().toString() : "");
			cellNumber++;
	
			Cell batchIdCell = assignCell(cellNumber, tableBodyStyle, row);
			batchIdCell.setCellValue(claimInvoiceFeed.getBatchId() != null ? claimInvoiceFeed.getBatchId().toString() : "");
			cellNumber++;
	
			Cell claimNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			claimNumberCell.setCellValue(claimInvoiceFeed.getClaimNumber() != null ? claimInvoiceFeed.getClaimNumber() : "");
			cellNumber++;
	
			Cell awardNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			awardNumberCell.setCellValue(claimInvoiceFeed.getAwardNumber() != null ? claimInvoiceFeed.getAwardNumber() : "");
			cellNumber++;
	
			Cell accountNumberCell = assignCell(cellNumber, tableBodyStyle, row);
			accountNumberCell.setCellValue(claimInvoiceFeed.getAccountNumber() != null ? claimInvoiceFeed.getAccountNumber() : "");
			cellNumber++;
	
			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(claimInvoiceFeed.getFeedType() != null ? claimInvoiceFeed.getFeedType() :"");
			cellNumber++;
	
			Cell updatedByCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedByCell.setCellValue(claimInvoiceFeed.getUpdateUser() != null ? claimInvoiceFeed.getUpdateUser() :"System");
			cellNumber++;
	
			Cell updatedOnCell = assignCell(cellNumber, tableBodyStyle, row);
			updatedOnCell.setCellValue(claimInvoiceFeed.getUpdateTimeStamp() != null ? commonService.convertDateFormatBasedOnTimeZone(claimInvoiceFeed.getUpdateTimeStamp().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			cellNumber++;
	
			Cell statusCell = assignCell(cellNumber, tableBodyStyle, row);
			statusCell.setCellValue(claimInvoiceFeed.getFeedStatus() != null ? claimInvoiceFeed.getFeedStatus() : "");
			cellNumber++;
		
			Cell followUpCell = assignCell(cellNumber, tableBodyStyle, row);
			followUpCell.setCellValue(claimInvoiceFeed.getUserAction() != null ? claimInvoiceFeed.getUserAction() : "");
			cellNumber++;
		
			Cell commentCell = assignCell(cellNumber, tableBodyStyle, row);
			commentCell.setCellValue(claimInvoiceFeed.getUserActionComment() != null ? claimInvoiceFeed.getUserActionComment() : "");
			cellNumber++;
			rowNumber++;
		}
	}

}
