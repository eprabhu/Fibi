/**
 * Created by Saranya T Pillai
 * Last Updated by Mahesh Sreenath V M - 31/03/2021
 */
import { Component, OnInit, OnDestroy } from '@angular/core';
import { ReportService } from '../report.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { CommonService } from '../../common/services/common.service';
import { HTTP_SUCCESS_STATUS, HTTP_ERROR_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';

@Component({
	selector: 'app-template-list',
	templateUrl: './template-list.component.html',
	styleUrls: ['./template-list.component.css']
})
export class TemplateListComponent implements OnInit, OnDestroy {
	templateList: any = [];
	filteredList: any = [];
	selectedTemplateType = 'U';
	searchText: string;
	debounceTimer: any;
	sortType = null;
	sortOrder = true;
	deleteTemplateId: any;
	$subscriptions: Subscription[] = [];
	isReportAdmin = false;
	loginPersonId: string = this._commonService.getCurrentUserDetail('personID');
	activeTab = 'REPORT';

	constructor(private _reportService: ReportService,
		public _commonService: CommonService,
		private _navigationService: NavigationService) { }

	async ngOnInit() {
		this.setLatestReportType();
		this.isReportAdmin = await this._commonService.checkPermissionAllowed('REPORT_ADMINISTATOR');
		this.fetchAllReportTemplates();
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}
	/**
	 * returns all templates
	 * stringifies the returned data in order to avoid reference problem
	 */
	fetchAllReportTemplates() {
		const requestObject = {
			'personId': this._commonService.getCurrentUserDetail('personID'),
			'isReportAdmin': this.isReportAdmin
		};
		this.$subscriptions.push(this._reportService.fetchAllReportTemplates(requestObject)
			.subscribe((data: any) => {
				// data.reportTemplates.map(R => { R.reportType.type = Math.floor(Math.random() * 2) ?  'B' : 'S';  });
				this.templateList = JSON.parse(JSON.stringify(data.reportTemplates));
				this.setLatestReportType();
				this.filterListByType();
				if (!this.filteredList.length) {
					this.selectedTemplateType = 'S';
					this.filterListByType();
				}
			}));
	}

	/**
	 * To make the last selected option by user mapped to 'selectedTemplateType.
	 * the url is checked because the last selected value is only used if user comes back from
	 * detailed report view. other routes are ignored. also default behavior is to set user template if there is any.
	 */
	setLatestReportType() {
		if (this._navigationService.previousURL.includes('fibi/dashboard/report/template')) {
			this.selectedTemplateType = this._reportService.selectedTemplateType;
			this.activeTab = this._reportService.activeTab;
		}
	}

	filter() {
		if (this.debounceTimer) {
			clearTimeout(this.debounceTimer);
		}
		this.debounceTimer = setTimeout(() => {
			const REPORT_TYPE = this.activeTab === 'REPORT' ? 'S' : 'B';
			this.filteredList = this.templateList.filter(template =>
				(template.templateDescription + template.templateName).toLowerCase()
				.includes(this.searchText.toLowerCase()) &&
				template.templateType === this.selectedTemplateType &&
				(template.reportType.type === REPORT_TYPE   || REPORT_TYPE === 'S' && !template.reportType.type));
		}, 500);
	}

	filterListByType() {
		this.searchText = '';
		this.sortType = null;
		const REPORT_TYPE = this.activeTab === 'REPORT' ? 'S' : 'B';
		this._reportService.selectedTemplateType = this.selectedTemplateType;
		this.filteredList = this.templateList.filter(template => template.templateType === this.selectedTemplateType
			 && (template.reportType.type === REPORT_TYPE   || REPORT_TYPE === 'S' && !template.reportType.type));
	}

	/**
	 * this sort works on the basis of which field is selected. the data used to compare are selected based
	 * on the field selected. There is a if else case used for 3 available fields value one and two will hold
	 * the data that need to compared and based on sortOrder the asc and desc is selected.
	 * DEV note: Didn't convert this into a sort pipe since its not reusable and the data used to sort is basically
	 * locale to this component so I thought ts will add more readability( by Mahesh).
	 */
	sortResult(sortFieldBy: string) {
		this.sortType = sortFieldBy;
		this.sortOrder = !this.sortOrder;
		this.filteredList.sort((a, b) => {
			let first = '';
			let second = '';
			if (sortFieldBy === 'module') {
				first = a.module.description.toLowerCase();
				second = b.module.description.toLowerCase();
			} else if (sortFieldBy === 'description') {
				first = a.templateDescription.toLowerCase();
				second = b.templateDescription.toLowerCase();
			} else {
				first = a.templateName.toLowerCase();
				second = b.templateName.toLowerCase();
			}
			if (first < second) {
				return this.sortOrder ? -1 : 1;
			}
			if (first > second) {
				return this.sortOrder ? 1 : -1;
			}
			return 0;
		});
	}

	deleteTemplate() {
		this.$subscriptions.push(this._reportService.deleteReportTemplate(this.deleteTemplateId)
			.subscribe((data: any) => {
				const deleteIndex = this.templateList.findIndex(template => template.reportTemplateId === this.deleteTemplateId);
				this.templateList.splice(deleteIndex, 1);
				this.deleteTemplateId = null;
				this.filterListByType();
			}, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Failed to delete Report'); },
			() => { this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Report deleted successfully'); }));
	}

	setActiveTab(tab: string): void {
		this.activeTab = tab;
		this._reportService.activeTab = tab;
	}
}

