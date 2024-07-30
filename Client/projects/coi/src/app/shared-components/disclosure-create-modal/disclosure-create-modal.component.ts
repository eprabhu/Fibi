import {Component, EventEmitter, HostListener, Input, OnInit, Output, } from '@angular/core';
import {Router} from '@angular/router';
import {Subscription} from 'rxjs';
import {
    getEndPointOptionsForDepartment,
    getEndPointOptionsForLeadUnit,
    getEndPointOptionsForProposalDisclosure,
    getEndPointOptionsForSponsor,getEndPointOptionsForCoiAwardNumber
} from '../../../../../fibi/src/app/common/services/end-point.config';
import {deepCloneObject, hideModal} from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import {subscriptionHandler} from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import {
    CREATE_DISCLOSURE_ROUTE_URL,
    CREATE_TRAVEL_DISCLOSURE_ROUTE_URL,
    HTTP_ERROR_STATUS,
    HTTP_SUCCESS_STATUS
} from '../../app-constants';
import {CommonService} from '../../common/services/common.service';
import {DisclosureCreateModalService} from './disclosure-create-modal.service';
import { RevisionObject, Disclosure } from '../shared-interface';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { checkForVowelInFirstLetter } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-disclosure-create-modal',
    templateUrl: './disclosure-create-modal.component.html',
    styleUrls: ['./disclosure-create-modal.component.scss'],
    providers: [DisclosureCreateModalService]
})

export class DisclosureCreateModalComponent implements OnInit {

    @Input() activeDisclosures;
    @Input() triggeredFrom: any;
    @Output() emitCreateOrRevise: EventEmitter<any> = new EventEmitter<any>();

    clearField = false;
    unitSearchOptions: any = {};
    isHideEndpointSearch = true;
    reviseObject: RevisionObject = new RevisionObject();
    isShowResultCard = false;
    clearProjectField: String;
    clearSponsorField: String;
    clearPrimeSponsorField: String;
    clearPIField: String;
    clearLUField: String;
    manualProjectAddDetails: any = {};
    projectDisclosureValidation = new Map();
    projectSearchOptions: any;
    isSearchExternalProject = false;
    $subscriptions: Subscription[] = [];
    piElasticSearchOptions: any = {};
    unitHttpOptions: any = {};
    primeSponsorSearchOptions: any = {};
    sponsorSearchOptions: any = {};
    projectTypes = [];
    selectedProjectType = null;
    existingDisclosureDetails: Disclosure = new Disclosure();
    isShowExistingDisclosure = false;
    mandatoryList = new Map();
    disclosureNumber: any;
    hasFCOI: any;
    canReviseFCOI: any;
    homeUnitName: null;
    title = '';
    isShowConcurrencyWarning = false;
    searchHelpText = '';
    unitHelpText = `To disclose at any other unit, please click on the 'Edit' icon near the unit field, and proceed to disclosure creation.
                    To revert to the original unit, click on the 'Reset' icon.`
    travelDescHelpText = 'Please provide the purpose of the trip.';
    projectTypeHelpText = 'Please select a project type to proceed with disclosure creation.';
    projectTitle: string = '';

    constructor(public commonService: CommonService, private _disclosureCreateModalService: DisclosureCreateModalService,
                private _router: Router, private _elasticConfig: ElasticConfigService) {
    }

    ngOnInit() {
        this.setSearchOptions();
        if (this.triggeredFrom === 'FCOI_DISCLOSURE') {
            this.checkForFCOIActive();
            this.openDisclosureCreateModal();
        } else if (this.triggeredFrom == 'TRAVEL_DISCLOSURE') {
            this.openDisclosureCreateModal();
        } else {
            this.getCoiProjectTypes();
        }
    }

    private openDisclosureCreateModal(): void {
        document.getElementById('open-create-or-revise-modal')?.click();
    }

    clearModal(): void {
        setTimeout(() => {
            this.mandatoryList.clear();
            this.clearProjectDisclosure();
            this.reviseObject = new RevisionObject();
            this.emitCreateOrRevise.emit({ closeModal: false });
            this.clearProjectField = new String('true');
        }, 200);
    }

    resetHomeUnit(): void {
        this.isHideEndpointSearch = true;
        this.reviseObject.homeUnit = this.commonService.currentUserDetails.homeUnit;
        this.unitSearchOptions.defaultValue = this.commonService.currentUserDetails.homeUnit + ' - ' + this.commonService.currentUserDetails.homeUnitName;
        this.mandatoryList.delete('homeUnit');
    }

    selectedUnitEvent(event): void {
        if (event) {
            this.reviseObject.homeUnit = event.unitNumber;
            this.homeUnitName = event.unitName;
        } else {
            this.reviseObject.homeUnit = null;
            this.homeUnitName = null;
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    validateSelectedProject(event: any): void {
        if (event) {
            this.projectDisclosureValidation.clear();
            this.mandatoryList.clear();
            const selectedModuleCode = this.selectedProjectType === 'Award' ? '1' : '3';
            // const moduleItemId = event ? this.selectedProjectType === 'Award' ? event.awardId : event.moduleItemId : null;
            const moduleItemId = event ? this.selectedProjectType === 'Award' ? event.moduleItemId : event.moduleItemId : null;
            const projectTitle = event ? `${event.moduleItemKey} ${event.title}` : '';
            this._disclosureCreateModalService.checkIfDisclosureAvailable(selectedModuleCode, moduleItemId).subscribe((data: any) => {
                if (data) {
                    if (data.pendingProject != null) {
                        this.isShowExistingDisclosure = true;
                        this.setExistingDisclosureDetails('Project', data.pendingProject, projectTitle);
                    } else if (data.fcoiProject != null) {
                        this.isShowExistingDisclosure = true;
                        this.setExistingDisclosureDetails('FCOI', data.fcoiProject, projectTitle);
                    } else {
                        this.assignSelectedProject(event);
                    }
                }
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, (err.error && err.error.errorMessage) ?
                    err.error.errorMessage : 'Error in selecting ' + this.selectedProjectType + '. Please try again.');
                this.changeProjectType();
            });
        } else {
            this.clearProjectDisclosure();
        }
    }

    createOrReviseDisclosure(): void {
        !this.hasFCOI ? this.createFCOIDisclosure() : this.reviseDisclosure();
    }

    clearProjectDisclosure(): void {
        // this.clearProjectField = new String('true');
        this.clearSponsorField = new String('true');
        this.clearPrimeSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.manualProjectAddDetails = {};
        this.manualProjectAddDetails.moduleItemId = null;
        this.manualProjectAddDetails.title = null;
        this.projectDisclosureValidation.clear();
        this.isShowResultCard = false;
        this.existingDisclosureDetails = null;
        this.isShowExistingDisclosure = false;
    }

    createProjectDisclosureAPI(): void {
        if (this.validateProject()) {
            this.$subscriptions.push(this._disclosureCreateModalService.createDisclosure(this.getCreateProjectRequestObject()).subscribe((data: any) => {
                if (data) {
                    hideModal('reviseOrCreateDisclosureModal');
                    this._router.navigateByUrl('/', { skipLocationChange: true }).then(() => {
                        this._router.navigate([CREATE_DISCLOSURE_ROUTE_URL], {queryParams: {disclosureId: data.coiDisclosure.disclosureId}});
                    });
                    this.clearModal();
                }
            }, err => {
                if (err.status === 405) {
                    this.isShowConcurrencyWarning = true;
                        this.setExistingDisclosureDetails('Project', err.error);
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, (err.error && err.error.errorMessage) ?
                        err.error.errorMessage : 'Error in creating project disclosure. Please try again.');
                }
            }));
        }
    }

    private validateTravelDisclosure(): boolean {
        if (!this.reviseObject.homeUnit) {
            this.mandatoryList.set('homeUnit', 'Please enter a valid unit to create a Travel disclosure.');
        }
        return this.mandatoryList.size === 0 ? true : false;
    }

    private getCreateTravelRequestObject(): void {
        sessionStorage.setItem('travelCreateModalDetails', JSON.stringify(
            {
                homeUnit: this.reviseObject.homeUnit ? this.reviseObject.homeUnit : null,
                description: this.reviseObject.revisionComment,
                personId: this.commonService.getCurrentUserDetail('personID'),
                homeUnitName: this.homeUnitName
            }
        ));
    }

    navigateToTravelDisclosure(): void {
        if (this.validateTravelDisclosure()) {
            hideModal('reviseOrCreateDisclosureModal');
            if (this._router.url.includes('create-travel-disclosure')) {
                this.commonService.$globalEventNotifier.next(
                    {
                        uniqueId: 'CREATE_NEW_TRAVEL_DISCLOSURE',
                        content: {
                            homeUnit: this.reviseObject.homeUnit ? this.reviseObject.homeUnit : null,
                            description: this.reviseObject.revisionComment,
                            personId: this.commonService.getCurrentUserDetail('personID'),
                            homeUnitName: this.homeUnitName
                        }
                    });
            } else {
                this.getCreateTravelRequestObject();
                this._router.navigate([CREATE_TRAVEL_DISCLOSURE_ROUTE_URL], { queryParams: { disclosureId: null }, queryParamsHandling: 'merge'});
            }
            this.clearModal();
        }
    }

    getCreateProjectRequestObject(): any {
        if (this.isSearchExternalProject) {
            return {
                coiDisclosure: {
                    homeUnit: this.reviseObject.homeUnit,
                    revisionComment: this.reviseObject.revisionComment,
                    coiProjectTypeCode: this.getCoiProjectTypeFromCode(),
                    moduleItemKey: this.manualProjectAddDetails.moduleItemId,
                    personId: this.commonService.getCurrentUserDetail('personID'),
                },
                [this.selectedProjectType == 'Award' ? 'coiProjectAward' : 'coiProjectProposal']: {
                    coiProjectTypeCode: this.getCoiProjectTypeFromCode(), ...this.getCreateDisclosureRO()
                }
            };
        } else {
            return {
                coiDisclosure: {
                    homeUnit: this.reviseObject.homeUnit,
                    coiProjectTypeCode: this.getCoiProjectTypeFromCode(),
                    revisionComment: this.reviseObject.revisionComment,
                    moduleItemKey: this.manualProjectAddDetails.moduleItemId,
                    personId: this.commonService.getCurrentUserDetail('personID')
                }
            };
        }
    }

    createFCOIDisclosure(): void {
        const fcoiDisclosureObj = {
            fcoiTypeCode: '1',
            homeUnit: this.reviseObject.homeUnit,
            revisionComment: this.reviseObject.revisionComment,
            personId: this.commonService.getCurrentUserDetail('personID')
        };
        if (this.validateForm()) {
            this._disclosureCreateModalService.createDisclosure({'coiDisclosure': fcoiDisclosureObj}).subscribe((data: any) => {
                hideModal('reviseOrCreateDisclosureModal');
                this._router.navigate([CREATE_DISCLOSURE_ROUTE_URL], {queryParams: {disclosureId: data.coiDisclosure.disclosureId}});
                this.clearModal();
            }, err => {
                if (err.status === 405) {
                    this.isShowConcurrencyWarning = true;
                        this.setExistingDisclosureDetails('FCOI', err.error);
                } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, (err.error && err.error.errorMessage) ?
                    err.error.errorMessage : 'Error in creating new FCOI. Please try again.');
                }
            });
        }
    }

    changeProjectType(): void {
        this.clearProjectDisclosure();
        this.clearProjectField = new String('true');
        switch (this.selectedProjectType) {
            case 'Award': {
                this.searchHelpText = '';
                setTimeout(() => {
                    this.searchHelpText = 'Please search and link an award.';
                });
                return this.projectSearchOptions = getEndPointOptionsForCoiAwardNumber(this.commonService.baseUrl);
            }
            case 'Development Proposal': {
                this.searchHelpText = '';
                setTimeout(() => {
                    this.searchHelpText = 'Please search and link a development proposal.';
                });
                return this.projectSearchOptions = getEndPointOptionsForProposalDisclosure(this.commonService.baseUrl);
            }
            default:
                this.selectedProjectType = null;
                break;
        }
    }

    switchExternalProject(isExternal: boolean): void {
        this.isSearchExternalProject = isExternal;
        this.resetManualProjectAddFields();
        this.clearProjectField = new String('true');
        this.changeProjectType();
        this.isShowResultCard = false;
    }

    onPISelect(selectedValue: any): void {
        this.manualProjectAddDetails.piPersonId = selectedValue.prncpl_id;
        this.manualProjectAddDetails.piName = selectedValue.full_name;
    }

    onLeadUnitSelect(selectedValue: any): void {
        this.manualProjectAddDetails.leadUnitName = selectedValue.unitDetail;
    }

    onPrimeSponsorSelect(selectedValue: any): void {
        this.manualProjectAddDetails.primeSponsorName = selectedValue.sponsorName;
    }

    onSponsorSelect(selectedValue: any): void {
        this.manualProjectAddDetails.sponsorName = selectedValue.sponsorName;
    }

    getDisclosureConflictBadge(statusCode: string): string {
        switch (String(statusCode)) {
            case '1':
                return 'green-badge';
            case '2':
                return 'brown-badge';
            case '3':
                return 'red-badge';
            case '4':
                return 'green-badge';
        }
    }



    getDispositionStatusBadge(statusCode): string {
        switch (statusCode) {
            case '1':
                return 'yellow-badge';
            case '2':
            case '4':
            case '5':
                return 'blue-badge';
            case '3':
            case '6':
                return 'green-badge';
            default:
                return 'yellow-badge';
        }
    }

    navigateToDisclosure(disclosureId): void {
        hideModal('reviseOrCreateDisclosureModal');
        this._router.navigate([CREATE_DISCLOSURE_ROUTE_URL], {
            queryParams: {
                disclosureId: disclosureId
            }
        });
    }

    private setSearchOptions(): void {
        this.unitSearchOptions = getEndPointOptionsForLeadUnit(this.commonService.currentUserDetails.homeUnit + '-' + this.commonService.currentUserDetails.homeUnitName, this.commonService.fibiUrl, 'unitNumber - unitName');
        this.reviseObject.homeUnit = this.commonService.currentUserDetails.homeUnit;
        this.homeUnitName = this.commonService.currentUserDetails.homeUnitName;
        this.piElasticSearchOptions = this._elasticConfig.getElasticForPerson();
        this.unitHttpOptions = getEndPointOptionsForDepartment();
        this.sponsorSearchOptions = getEndPointOptionsForSponsor();
        this.primeSponsorSearchOptions = getEndPointOptionsForSponsor();
    }

    private checkForFCOIActive(): void {
        this.hasFCOI = this.activeDisclosures.find(disclosure =>
            disclosure.fcoiTypeCode == '1'
        );
        this.canReviseFCOI = this.activeDisclosures.find(disclosure =>
            disclosure.fcoiTypeCode == '1' && disclosure.coiReviewStatusType.reviewStatusCode != '4'
        );
        this.disclosureNumber = this.hasFCOI ? this.hasFCOI.disclosureNumber : null;
    }

    private validateForm(): boolean {
        this.mandatoryList.clear();
        if (!this.reviseObject.revisionComment && this.hasFCOI) {
            this.mandatoryList.set('reviseComment', 'Please enter the reason for revising your FCOI disclosure.');
        }
        if (!this.reviseObject.homeUnit) {
            this.mandatoryList.set('homeUnit', 'Please select a valid unit to create an FCOI disclosure.');
        }
        return this.mandatoryList.size !== 0 ? false : true;
    }

    private getCoiProjectTypes(): void {
        this.$subscriptions.push(this._disclosureCreateModalService.getCoiProjectTypes().subscribe((res: any) => {
            this.projectTypes = res.coiProjectTypes;
            this.openDisclosureCreateModal();
        }));
    }

    private reviseDisclosure(): void {
        this.reviseObject.disclosureId = this.hasFCOI ? this.hasFCOI.disclosureId : null;
        if (this.validateForm()) {
            if (!this.canReviseFCOI) {
                this.$subscriptions.push(this._disclosureCreateModalService.reviseDisclosure(this.reviseObject)
                    .subscribe((data: any) => {
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'New version of disclosure created.');
                        hideModal('reviseOrCreateDisclosureModal');
                        this._router.navigate([CREATE_DISCLOSURE_ROUTE_URL],
                            {queryParams: {disclosureId: data.coiDisclosure.disclosureId}});
                        this.clearModal();
                    }, err => {
                        if (err.status === 405) {
                            this.isShowConcurrencyWarning = true;
                                this.setExistingDisclosureDetails('FCOI', err.error);
                        } else {
                        this.commonService.showToast(HTTP_ERROR_STATUS, (err.error && err.error.errorMessage) ?
                            err.error.errorMessage : 'Error in creating new version. Please try again.');

                    }}));
            } else {
                this.isShowExistingDisclosure = true;
                this.setReviseDisclosure();
            }
        }
    }

    private setReviseDisclosure(): void {
        this.existingDisclosureDetails.disclosureType = 'FCOI';
        this.existingDisclosureDetails.adminGroupId = this.canReviseFCOI.adminGroupId;
        this.existingDisclosureDetails.adminPersonId = this.canReviseFCOI.adminPersonId;
        this.existingDisclosureDetails.certifiedAt = this.canReviseFCOI.certifiedAt;
        this.existingDisclosureDetails.createTimestamp = this.canReviseFCOI.createTimestamp;
        this.existingDisclosureDetails.reviewStatus = this.canReviseFCOI.coiReviewStatusType ? this.canReviseFCOI.coiReviewStatusType.description : null;
        this.existingDisclosureDetails.reviewStatusCode = this.canReviseFCOI.coiReviewStatusType ? this.canReviseFCOI.coiReviewStatusType.reviewStatusCode : null;
        this.existingDisclosureDetails.createUserFullName = this.canReviseFCOI.createUserFullName;
        this.existingDisclosureDetails.disclosureId = this.canReviseFCOI.disclosureId;
        this.existingDisclosureDetails.disclosureNumber = this.canReviseFCOI.disclosureNumber;
        this.existingDisclosureDetails.dispositionStatus = this.canReviseFCOI.coiDispositionStatusType ? this.canReviseFCOI.coiDispositionStatusType.description : null;
        this.existingDisclosureDetails.dispositionStatusCode = this.canReviseFCOI.coiDispositionStatusType ? this.canReviseFCOI.coiDispositionStatusType.dispositionStatusCode : null;
        this.existingDisclosureDetails.conflictStatus = this.canReviseFCOI.coiConflictStatusType ? this.canReviseFCOI.coiConflictStatusType.description : null;
        this.existingDisclosureDetails.conflictStatusCode = this.canReviseFCOI.coiConflictStatusType ? this.canReviseFCOI.coiConflictStatusType.coiConflictStatusCode : null;
        this.existingDisclosureDetails.expirationDate = this.canReviseFCOI.expirationDate;
        this.existingDisclosureDetails.homeUnit = this.canReviseFCOI.homeUnit;
        this.existingDisclosureDetails.homeUnitName = this.canReviseFCOI.homeUnit ? this.canReviseFCOI.unit.unitName : null;
        this.existingDisclosureDetails.personId = this.canReviseFCOI.personId;
        this.existingDisclosureDetails.updateTimestamp = this.canReviseFCOI.updateTimestamp;
        this.existingDisclosureDetails.updateUserFullName = this.canReviseFCOI.updateUserFullName;
        this.existingDisclosureDetails.versionNumber = this.canReviseFCOI.versionNumber;
        this.existingDisclosureDetails.versionStatus = this.canReviseFCOI.versionStatus;
        this.existingDisclosureDetails.disclosurePersonFullName = this.canReviseFCOI.disclosurePersonFullName;
    }

    private setExistingDisclosureDetails(type: string, data: any, projectTitle: string = ''): void {
        this.disclosureNumber = data.disclosureNumber;
        this.existingDisclosureDetails = deepCloneObject(data);
        this.existingDisclosureDetails['disclosureType'] = type;
        if (type == 'Project') {
            this.existingDisclosureDetails['type'] = this.selectedProjectType;
            this.projectTitle = projectTitle;
        }
        this.isShowResultCard = true;
    }

    private assignSelectedProject(event): void {
        if (event) {
            this.manualProjectAddDetails.moduleItemId = this.selectedProjectType == 'Award' ? event.moduleItemId : event.moduleItemId;
            this.manualProjectAddDetails.title = (this.selectedProjectType == 'Award' ? event.moduleItemKey : event.moduleItemId)  + ' - ' + event.title ;
            this.manualProjectAddDetails.principalInvestigator = this.selectedProjectType == 'Award' ? event.principalInvestigator : event.principalInvestigator;
            this.manualProjectAddDetails.unitName = this.selectedProjectType == 'Award' ? event.unitName : event.unitName;
            this.manualProjectAddDetails.startDate = this.selectedProjectType == 'Award' ? event.startDate : event.startDate;
            this.manualProjectAddDetails.endDate = this.selectedProjectType == 'Award' ? event.endDate : event.endDate;
            this.manualProjectAddDetails.sponsor = this.selectedProjectType == 'Award' ? event.sponsor : event.sponsor;
            this.manualProjectAddDetails.reporterRole = this.selectedProjectType =='Award' ? event.reporterRole : event.reporterRole;
            this.manualProjectAddDetails.primeSponsorName = this.selectedProjectType == 'Award' ? event.primeSponsor : event.primeSponsor;
            this.isShowResultCard = true;
        }
    }

    private getCreateDisclosureRO(): any {
        const RO = {
            ...this.manualProjectAddDetails,
            [this.selectedProjectType == 'Award' ? 'awardNumber' : 'proposalNumber']: this.manualProjectAddDetails.moduleItemId
        };
        delete RO.moduleItemId;
        return RO;
    }

    private getCoiProjectTypeFromCode(description = this.selectedProjectType): void {
        return this.projectTypes.find(type => type.description === description).coiProjectTypeCode;
    }

    private validateProject(): boolean {
        this.projectDisclosureValidation.clear();
        this.mandatoryList.clear();
        if (!this.selectedProjectType) {
            this.projectDisclosureValidation.set('projectSelect', 'Please select any one of the given Project Type');
        }
        if (!this.manualProjectAddDetails || !this.manualProjectAddDetails.moduleItemId) {
            this.projectDisclosureValidation.set('proposalSearch', 'Please select '+ checkForVowelInFirstLetter(this.selectedProjectType) + ' to create disclosure.');
        }
        if (!this.reviseObject.homeUnit) {
            this.mandatoryList.set('homeUnit', 'Please enter a valid unit to create a Project disclosure.');
        }
        return this.projectDisclosureValidation.size === 0 && this.mandatoryList.size === 0 ? true : false;
    }

    private resetManualProjectAddFields(): void {
        this.clearSponsorField = new String('true');
        this.clearPIField = new String('true');
        this.clearLUField = new String('true');
        this.manualProjectAddDetails = {};
        this.manualProjectAddDetails.moduleItemId = null;
        this.manualProjectAddDetails.title = null;
    }

    private setHelpTextToolTip(): string {
        if (this.triggeredFrom === 'FCOI_DISCLOSURE' && !this.isShowExistingDisclosure) {
            this.hasFCOI ? this.title = 'Create Revision help text' : this.title = 'Create FCOI help text';
        } else if (this.triggeredFrom === 'PROJECT_DISCLOSURE' && !this.isShowExistingDisclosure) {
            this.title = 'Create Project Disclosure help text';
        } else if (this.triggeredFrom === 'TRAVEL_DISCLOSURE') {
            this.title = 'Create Travel Disclosure help text';
        }
        return this.title;
    }

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            this.clearModal();
        }
    }

}
