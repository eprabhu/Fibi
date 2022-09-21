/** Last updated by Aravind  on 13-01-2020 */

import { Component, OnInit, ViewChild, ElementRef, OnDestroy, HostListener, Output, EventEmitter, Input } from '@angular/core';

import { Router } from '@angular/router';
import { InstituteProposalService } from './services/institute-proposal.service';
import { subscriptionHandler } from '../common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../common/services/common.service';
import { ActionType, BRValidation, InstituteProposal } from './institute-proposal-interfaces';
import { DataStoreService } from './services/data-store.service';
import { fileDownloader, openInNewTab } from '../common/utilities/custom-utilities';
import { concatUnitNumberAndUnitName } from '../common/utilities/custom-utilities';
import { NavigationService } from '../common/services/navigation.service';
import { HTTP_ERROR_STATUS } from '../app-constants';

declare var $: any;

@Component({
  selector: 'app-institute-proposal',
  templateUrl: './institute-proposal.component.html',
  styleUrls: ['./institute-proposal.component.css']
})
export class InstituteProposalComponent implements OnInit, OnDestroy {

  @Input() reviewTypes: any = {};
  @Input() specialReviewApprovalTypes: any = {};

  @ViewChild('moreOptionsBtn', { static: false }) moreOptions: ElementRef;

  @ViewChild('mainHeaders', { static: true }) mainHeaders: ElementRef;
  result = new InstituteProposal();
  $subscriptions: Subscription[] = [];
  canCreateAward = false;
  isShowMoreOptions = false;
  isShowReviewActions = false;
  currentActionType: ActionType = new ActionType();
  isSubmitEnabled = false;
  isAdminCorrectionEnabled = false;
  description = '';
  BRValidation: BRValidation = new BRValidation();
  concatUnitNumberAndUnitName = concatUnitNumberAndUnitName;
  printTemplates: any = null;
  validationMap = new Map();
  isDownloading = false;
  currentTemplateArr: any = [];
  isChecked = {};

  constructor(public _commonService: CommonService,
    private _router: Router,
    public _instituteService: InstituteProposalService,
    private _dataStore: DataStoreService,
    private _navigationService: NavigationService) {
    document.addEventListener('mouseup', this.offClickHandler.bind(this));
    document.addEventListener('mouseup', this.offClickMainHeaderHandler.bind(this));
  }
  // The function is used for closing nav dropdown at mobile screen
  offClickMainHeaderHandler(event: any) {
    if (window.innerWidth < 992) {
      const ELEMENT = <HTMLInputElement>document.getElementById('navbarResponsive');
      if (!this.mainHeaders.nativeElement.contains(event.target)) {
        if (ELEMENT.classList.contains('show')) {
          document.getElementById('responsiveColapse').click();
        }
      }
    }
  }
  ngOnInit() {
    this.result = this._instituteService.instituteProposalData.value;
    this._dataStore.setInstituteProposal(this.result);
    this.setProposalCurrentDateValues();
    this.setButtonStatus();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  setProposalCurrentDateValues() {
    this._dataStore.currentStartDate = this.result.instProposal.startDate;
    this._dataStore.currentEndDate = this.result.instProposal.endDate;
    this._instituteService.ipTitle = this.result.instProposal.title;
  }

  setButtonStatus() {
    this.isSubmitEnabled = this.result.instProposal.proposalSequenceStatus === 'PENDING' &&
      this.result.availableRights.includes('MODIFY_INST_PROPOSAL');
    this.canCreateAward = this.result.availableRights.includes('CREATE_AWARD') &&
      this.result.instProposal.proposalSequenceStatus !== 'PENDING';
    this.isAdminCorrectionEnabled = this.result.instProposal.proposalSequenceStatus !== 'PENDING' &&
      this.result.availableRights.includes('MODIFY_INST_PROPOSAL') && !this.result.isAwarded
      && this._commonService.isDevProposalVersioningEnabled;
  }

  getBadgeByStatusCode(statusCode) {
    if (statusCode === 1 || statusCode === 6) {
      return 'warning';
    } else if (statusCode === 3 || statusCode === 4 || statusCode === 5) {
      return 'danger';
    } else if (statusCode === 2) {
      return 'success';
    } else {
      return 'info';
    }
  }

  openGoBackModal() {
    this._router.navigate(['/fibi/dashboard/instituteProposalList']);
  }

  setProposalCurrentTab() {
    localStorage.setItem('currentTab', 'PROPOSAL_REVIEW');
  }

  offClickHandler(event: any) {
    if (this.moreOptions) {
      if (!this.moreOptions.nativeElement.contains(event.target)) {
        this.isShowMoreOptions = false;
        this.isShowReviewActions = false;
      }
    }
  }

  @HostListener('window:scroll', ['$event'])
  onWindowScroll() {
    const HEIGHT = document.getElementById('stickyIpHeader').offsetHeight;
    const HEADER = document.getElementById('stickyIpHeader');
    if (window.pageYOffset > HEIGHT && this._commonService.isIE) {
      HEADER.classList.add('tab-sticky');
    } else {
      HEADER.classList.remove('tab-sticky');
    }
  }

  createAdminCorrection() {
    this.$subscriptions.push(this._instituteService.createNewIPVersion(
      { proposalId: this.result.instProposal.proposalId, description: this.description })
      .subscribe((data: InstituteProposal) => {
        this.description = '';
        this.result.instProposal = data.instProposal;
        this.result.instituteProposalPersons = data.instituteProposalPersons;
        this.result.instituteProposalKeywords = data.instituteProposalKeywords;
        this.result.instituteProposalPersons = data.instituteProposalPersons;
        this.result.instituteProposalPersons = data.instituteProposalPersons;
        this.result.instituteProposalResearchAreas = data.instituteProposalResearchAreas;
        this.result.proposalId = data.proposalId;
        this._dataStore.setInstituteProposal(this.result);
        this._dataStore.dataEvent.next(Object.keys(this.result));
        this.setButtonStatus();
        this._router.navigate(['fibi/instituteproposal/overview'],
          { queryParams: { 'instituteProposalId': this.result.instProposal.proposalId } });
      }));
  }

  changeIPStatus(): void {
    const data = {
      instituteProposalActionType: this.currentActionType,
      proposalId: this.result.instProposal.proposalId,
      description: this.description
    };
    this.$subscriptions.push(this._instituteService.changeIpStatus(data)
      .subscribe((res: InstituteProposal) => {
        this.description = '';
        this.result.instProposal.instProposalStatus = this.currentActionType.instProposalStatus;
        this.result.instProposal.statusCode = this.currentActionType.instProposalStatus.statusCode;
        this._dataStore.updateStoreData({ 'instProposal': this.result.instProposal });
      }));
  }

  submitAdminCorrection() {
    this.$subscriptions.push(this._instituteService.submitIPVersion(
      { proposalId: this.result.instProposal.proposalId, description: this.description })
      .subscribe((data: InstituteProposal) => {
        this.description = '';
        this.result.instProposal.proposalSequenceStatus = 'ACTIVE';
        this._dataStore.setInstituteProposal(this.result);
        this.setButtonStatus();
        this._dataStore.dataEvent.next(Object.keys(this.result));
        this._instituteService.isInstituteProposalDataChange = false;
      }));
  }

  getVersionStatusCode(statusCode) {
    if (statusCode === 'ACTIVE') {
      return 'success';
    } else if (statusCode === 'PENDING') {
      return 'warning';
    } else {
      return 'info';
    }
  }

  navigateUsingRedirectRoute() {
    this._instituteService.isInstituteProposalDataChange = false;
    this.redirectBasedOnQueryParam();
  }

  redirectBasedOnQueryParam() {
    this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
  }

  checkForUnsavedChanges() {
    if (this._instituteService.isInstituteProposalDataChange) {
      $('#ipSubmitWithoutSaveModal').modal('show');
    } else {
      this.evaluateValidation();
    }
  }

  async evaluateValidation() {
    this.BRValidation = new BRValidation();
    const validationRequest: any = {
      moduleCode: 2,
      subModuleCode: 0,
      moduleItemKey: this.result.instProposal.proposalId.toString(),
      subModuleItemKey: 0,
    };
    const data: any = await this._instituteService.evaluateValidation(validationRequest);
    if (data && data.length > 0) {
      data.forEach(M => (M.validationType === 'VW') ?
        this.BRValidation.warning.push(M) : this.BRValidation.error.push(M));
      $('#BR_MODAL').modal('show');
    } else {
      $('#ConfirmSubmitModal').modal('show');
    }
  }

  showDevProposals(): void {
    this.result.devProposalIds.length > 1 ? $('#devProposalListModal').modal('show') :
    this.navigateToDevProposal(this.result.devProposalIds[0]);
  }

  navigateToDevProposal(proposalId: number): void {
    openInNewTab('proposal/summary?', ['proposalId'], [proposalId]);
    this.setProposalCurrentTab();
  }

  getPrintTemplates() {
    if (this.printTemplates) {
      $('#printIPModal').modal('show');
      return null;
    }
    this.$subscriptions.push(this._instituteService.getLetterTemplates().subscribe(
      (res: any) => {
        this.printTemplates = res.data;
        $('#printIPModal').modal('show');
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching print templates failed. Please try again');
      }
    ));
  }

  initiateDownload() {
    if (this.currentTemplateArr.length) {
      this.printIPAsZipOrDocxOrPdf(this.currentTemplateArr.length === 1 ? this.currentTemplateArr[0].printFileType : 'zip');
    } else {
      this.validationMap.set('selectTemplate', 'Please select one template');
    }
  }

  selectedTemplates(isChecked: any, template: any) {
    if (isChecked) {
      this.currentTemplateArr.push(template);
    } else {
      const INDEX = this.currentTemplateArr.findIndex(element => element.letterTemplateTypeCode === template.letterTemplateTypeCode);
      this.currentTemplateArr.splice(INDEX, 1);
    }
  }

  printIPAsZipOrDocxOrPdf(fileType: string) {
    if (!this.isDownloading) {
      this.isDownloading = true;
      this.$subscriptions.push(this._instituteService
        .printInstituteProposal({
          'instituteProposalId': this.result.instProposal.proposalId,
          'letterTemplateTypeCodes': this.setTypeCodeArray()
        }).subscribe(
          data => {
            this.closePrintModal();
            this.parsePrintedPage(data, fileType);
            this.isDownloading = false;
          }, (err) => {
            this.closePrintModal();
            setTimeout(() => {
              this._commonService.showToast(HTTP_ERROR_STATUS, 'Printing institute proposal failed. Please try again.');
            }, 500);
            this.isDownloading = false;
          }
        ));
    }
  }

  setTypeCodeArray() {
    return this.currentTemplateArr.map(template => template.letterTemplateTypeCode);
  }

  parsePrintedPage(data, fileType: string) {
    const person_name = this.result.instProposal.principalInvestigator ? this.result.instProposal.principalInvestigator : null;
    const fileName = 'Institute Proposal_' + this.result.instProposal.proposalId + '_' + person_name;
    fileDownloader(data, fileName, fileType);
  }

  closePrintModal() {
    $('#printIPModal').modal('hide');
    this.currentTemplateArr = [];
    this.isChecked = {};
  }

}

