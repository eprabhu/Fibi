package com.polus.fibicomp.notification.render;

import java.util.HashMap;
import java.util.Map;

import javax.transaction.Transactional;

import com.polus.fibicomp.award.pojo.ReportClass;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.progressreport.dao.ProgressReportDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;

@Transactional
@Service
public class AwardProgressReportRenderService implements EmailRenderService {

	@Value("${spring.application.name}")
	private String applicationURL;

	@Autowired
	private ProgressReportDao progressReportDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardRenderService awardRenderService;

	@Autowired
	private CommonService commonService;

	@Override
	public Map<String, String> getPlaceHolderData(String moduleItemKey) {
		AwardProgressReport awardProgressReport = progressReportDao.loadAwardProgressReport(Integer.parseInt(moduleItemKey));
		Map<String, String> placeHolder = new HashMap<>();			
		Award award = awardDao.fetchAwardByAwardId(awardProgressReport.getAwardId().toString());
		placeHolder.putAll(awardRenderService.getAwardPlaceHolder(award));
		placeHolder.putAll(getProgressReportPlaceHolder(awardProgressReport));
		return placeHolder;
	}

	private Map<String, String> getProgressReportPlaceHolder(AwardProgressReport awardProgressReport) {
		Map<String, String> placeHolder = new HashMap<>();
		placeHolder.put("{PROGRESS_REPORT_NUMBER}", awardProgressReport.getProgressReportNumber());
		placeHolder.put("{DUE_DATE}", (awardProgressReport.getDueDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(awardProgressReport.getDueDate().getTime(),Constants.DEFAULT_DATE_FORMAT) + "" : "");
		placeHolder.put("{TITLE}", (awardProgressReport.getAward().getTitle() != null) ? awardProgressReport.getAward().getTitle() + "" : "");
		placeHolder.put("{PROGRESS_REPORT_TITLE}", (awardProgressReport.getTitle() != null) ? awardProgressReport.getTitle() + "" : "");
		placeHolder.put("{USER_NAME}", personDao.getUserFullNameByUserName(awardProgressReport.getUpdateUser()));
		placeHolder.put("{APPLICATION_URL}", generateLinkToApplication(awardProgressReport.getProgressReportId()));
		String reportType = "";
		if(awardProgressReport.getReportClassCode() != null) {
			ReportClass classCode = progressReportDao.getReportClass(awardProgressReport.getReportClassCode());
			reportType = classCode != null ? classCode.getDescription() : "";
		}
		placeHolder.put("{REPORT_TYPE}", reportType);
		return placeHolder;
	}

	@Override
	public String getModuleType() {
		return String.valueOf(Constants.PROGRESS_REPORT_MODULE_CODE);
	}

	@Override
	public String getSubModuleCode() {
		return Constants.PROGRESS_REPORT_SUBMODULE_CODE.toString();
	}

	private String generateLinkToApplication(Integer parameter) {
		return  Constants.APPLICATION_URL_START_TAG + applicationURL + Constants.APPLICATION_URL_PROGRESS_REPORT_PATH +parameter+Constants.APPLICATION_URL_END_TAG;
	}

}
