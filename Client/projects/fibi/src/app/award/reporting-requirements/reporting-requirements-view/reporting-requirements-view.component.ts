import { Component, OnDestroy, OnInit } from '@angular/core';
import { ReportingRequirementsService } from '../reporting-requirements.service';
import { CommonService } from '../../../common/services/common.service';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';
import { CommonDataService } from '../../services/common-data.service';
import { WafAttachmentService } from '../../../common/services/waf-attachment.service';
import { setHelpTextForSubItems } from '../../../common/utilities/custom-utilities';

declare var $: any;

@Component({
    selector: 'app-reporting-requirements-view',
    templateUrl: './reporting-requirements-view.component.html',
    styleUrls: ['./reporting-requirements-view.component.css']
})
export class ReportingRequirementsViewComponent implements OnInit, OnDestroy {

    reportsData: any = [];
    reportKeys: any[] = [];
    reportTermsLookup: any = {};
    awardId: any;
    awardReport: any = {
        awardReportTermRecipient: []
    };
    awardData: any = {};
    currentTab;
    isDetailsOpen = [];
    $subscriptions: Subscription[] = [];
    isProgressReportEnabled = false;
    isReplaceAttachmentEnabled = false;
    isEditEnabledForSection = false;
    helpText: any = {report: {reportClassCode: null, reportCode: null, parentHelpTexts: []}};

    constructor(private _reportTermsService: ReportingRequirementsService, private _route: ActivatedRoute,
                public _commonService: CommonService, private _elasticConfig: ElasticConfigService, public _commonData: CommonDataService,
                private _wafAttachmentService: WafAttachmentService) {
    }

    ngOnInit() {
        this.awardId = this._route.snapshot.queryParamMap.get('awardId');
        if (this.awardId) {
            this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
                if (data) {
                    this.awardData = data.award;
                    this.getReportTermsLookUp();
                    this.getHelpText();
                }
            }));
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getReportTermsLookUp() {
        this.$subscriptions.push(this._reportTermsService.reportsTermsLookUpData(this.awardId)
            .subscribe((result: any) => {
                this.reportTermsLookup = result;
            }));
        this.getReportsData();
    }

    getReportsData() {
        this.$subscriptions.push(this._reportTermsService.reportsData(this.awardId)
            .subscribe((data: any) => {
                this.reportsData = data.awardReportsList;
                this.isProgressReportEnabled = data.isProgressReportEnabled;
                this.isReplaceAttachmentEnabled = data.isReplaceAttachmentEnabled;
                this.isEditEnabledForSection = data.isEditEnabledForSection;
                if (this.reportsData) {
                    this.reportKeys = Object.keys(this.reportsData);
                    if (!this.currentTab) {
                        this.switchTabs(this.reportKeys[0]);
                    }
                }
            }));
    }

    getHelpText() {
        this.$subscriptions.push(this._reportTermsService
            .fetchHelpText({ 'moduleCode': 1, 'sectionCodes': [109] }).subscribe((res: any) => {
                if (res) {
                    this.helpText = res;
                    this.setHelpTextForSubItems();
                }
            }));
    }

    setHelpTextForSubItems() {
        if (Object.keys(this.helpText).length && this.helpText.report && this.helpText.report.parentHelpTexts.length) {
            this.helpText = setHelpTextForSubItems(this.helpText, 'report');
            if (this.helpText.report['reportClassCode'] && this.helpText.report['reportClassCode'].parentHelpTexts.length > 0) {
                this.helpText.report = setHelpTextForSubItems(this.helpText.report, 'reportClassCode');
            }
            if (this.helpText.report['reportCode'] && this.helpText.report['reportCode'].parentHelpTexts.length > 0) {
                this.helpText.report = setHelpTextForSubItems(this.helpText.report, 'reportCode');
            }
        }
    }

    /**
     * @param  {} frequencyTypeCode
     *  get Frequency type Code and returns corresponding type description to the table list
     */
    getFrequencyType(frequencyTypeCode) {
        let frequencyType: any = {};
        if (this.reportTermsLookup.frequencyList && frequencyTypeCode) {
            frequencyType = this.reportTermsLookup.frequencyList.find(type => type.frequencyCode === frequencyTypeCode);
            return String(frequencyType.description);
        }
    }

    /**
     * @param  {} frequencyBasisCode
     * get Frequencybasis Code and returns corresponding type description to the table list
     */
    getFrequencyBasis(frequencyBasisCode) {
        let frequencyBasis: any = {};
        if (this.reportTermsLookup.frequencyBaseList && frequencyBasisCode) {
            frequencyBasis = this.reportTermsLookup.frequencyBaseList.find(type => type.frequencyBaseCode === frequencyBasisCode);
            return String(frequencyBasis.description);
        }
    }


    switchTabs(tabName: string) {
        this.currentTab = tabName;
        this.isDetailsOpen = [];
    }

    toggleReportDetails(reportCodeList: any, index: number) {
        this.isDetailsOpen[index] = !this.isDetailsOpen[index];
    }

}
