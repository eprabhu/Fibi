import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class ReportService {

	endpointSearchOptions: any = {
		contextField: '',
		formatString: '',
		path: '',
		defaultValue: ''
	};
	currentSelectedtemplate = {};
	selectedTemplateType = 'S';
	activeTab = 'REPORT';

	constructor(private _commonService: CommonService, private _http: HttpClient) { }

	getReportJSON(moduleCode: number) {
		return this._http.post(this._commonService.baseUrl + '/getReportMetaData', { reportTypeId: moduleCode });
	}

	getTemplateDetails(params) {
		return this._http.post(this._commonService.baseUrl + '/getReportTemplateById', params);
	}

	generateReport(reportObject) {
		return this._http.post(this._commonService.baseUrl + '/generateReport', reportObject);
	}

	generateBirtReport(reportObject) {
		return this._http.post(this._commonService.baseUrl + '/generateReportFromBirt', reportObject, {
			observe: 'response',
			responseType: 'blob'
		});
	}

	exportReportData(params) {
		return this._http.post(this._commonService.baseUrl + '/exportGeneratedReport', params, {
			observe: 'response',
			responseType: 'blob'
		});
	}

	saveOrUpdateReportTemplate(params) {
		return this._http.post(this._commonService.baseUrl + '/saveOrUpdateReportTemplate', params);
	}

	fetchAllReportTemplates(params) {
		return this._http.post(this._commonService.baseUrl + '/fetchAllReportTemplates', params);
	}

	getReportCount(reportObject) {
		return this._http.post(this._commonService.baseUrl + '/getNumberOfRecords', reportObject);
	}

	deleteReportTemplate(templateId) {
		return this._http.post(this._commonService.baseUrl + '/deleteReportTemplate', { reportTemplateId: templateId });
	}

	getBirtReportParamaters(reportObject) {
		return this._http.post(this._commonService.baseUrl + '/getParameterDetails', {'reportTypeId': 93});
	}
}
