import {Component, ElementRef, EventEmitter, HostListener, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges, ViewChild} from '@angular/core';
import {deepCloneObject, hideModal, openModal} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import {interval, Subject} from 'rxjs';
import {listAnimation} from '../../../common/utilities/animations';
import {debounce} from 'rxjs/operators';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from '../../../app-constants';
import {CommonService} from '../../../common/services/common.service';
import {RelationshipService} from '../relationship.service';

@Component({
    selector: 'app-sfi-project-relation-shared',
    templateUrl: './sfi-project-relation-shared.component.html',
    styleUrls: ['./sfi-project-relation-shared.component.scss'],
    animations: [listAnimation]
})
export class SfiProjectRelationSharedComponent implements OnInit, OnChanges, OnDestroy {

    @Input() projectSFIDetails = [];
    @Input() coiStatusList = [];
    @Input() projectIdTitleMap = {};
    @Input() isSaving = false;
    @Input() isSlider = false;
    @Output() isSavingChange = new EventEmitter();
    @Output() relationshipChanged = new EventEmitter();

    coiValidationMap: Map<string, string> = new Map();
    coiTableValidation: Map<string, string> = new Map();
    coiStatusCode = null;
    coiDescription = '';
    conflictStatusMap = {};
    isEditMode = true;
    tempProjectSFIDetails = [];
    focusableId = '';
    $debounceEvent = new Subject<any>();
    $subscriptions = [];
    isApplyToAllModal = false;

    constructor(private _commonService: CommonService, private _relationShipService: RelationshipService) {
    }

    ngOnChanges(changes: SimpleChanges) {
        if (changes.projectSFIDetails && !changes.projectSFIDetails.isFirstChange()) {
            this.coiValidationMap.clear();
            this.coiTableValidation.clear();
        }
        this.projectSFIDetails = this.projectSFIDetails.map(ele =>({...ele,isSaved: false})); 
    }

    ngOnInit() {
        this.listenScreenSize();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    openSaveAllConfirmationModal() {
        this.coiValidationMap.clear();
        this.coiStatusCode = null;
        this.coiDescription = '';
        this.isApplyToAllModal = true;
        openModal('applyToAllConfirmationModal');
    }

    getStatusDescriptionByCode(code: string): string {
        if (this.conflictStatusMap[code]) {
            return this.conflictStatusMap[code].description || '';
        }
        const STATUS = this.coiStatusList.find(S => S.projectConflictStatusCode === code);
        this.conflictStatusMap[code] = STATUS;
        return STATUS ? STATUS.description : '';
    }

    sfiSingleSave(index:number, sfi:any, focusableId: string) {
        this.focusableId = focusableId;
        this.updateIsSaving(true);
        this.saveSingleEntity(index,sfi)
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(1000))).subscribe((data: any) => {
                if (data) {
                    this.saveSingleEntity(data.index, data.SFI);
                }
            }
        ));
    }

    // Function for saving the single entity
    saveSingleEntity(index, test) {
        this.coiTableValidation.delete('save-status' + index);
        this.coiTableValidation.delete('save-description' + index);
        if (this.projectSFIDetails[index].disclComment.comment) {
            this.projectSFIDetails[index].disclComment.comment = this.projectSFIDetails[index].disclComment.comment.trim();
        }
        if ([null, 'null'].includes(this.projectSFIDetails[index].projectConflictStatusCode)) {
            this.coiTableValidation.set('save-status' + index, 'Please select Conflict Status');
        }
        if (!this.projectSFIDetails[index].disclComment.comment) {
            this.coiTableValidation.set('save-description' + index, 'Please enter description');
        }
        if (!this.coiTableValidation.has('save-status' + index) && !this.coiTableValidation.has('save-description' + index)) {
            test.coiProjConflictStatusType = this.getStatusObject(test.projectConflictStatusCode);
            this.singleSaveClick(test, index);
        } else {
            this.updateIsSaving(false);
        }
    }

    getStatusObject(code) {
        return this.coiStatusList.find(ele => ele.projectConflictStatusCode === code);
    }


    singleSaveClick(element, index) {
        if (!this.projectSFIDetails[index].isSaved) {
            this.projectSFIDetails[index].isSaved = true;
            delete element.isSaved;
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Saving....', 1250);
            this.$subscriptions.push(this._relationShipService.singleEntityProjectRelationSFI(element, element.personEntityId,
                element.disclosureId, this._commonService.currentUserDetails.personId).subscribe((data: any) => {
                this._relationShipService.projectSFIDetails[element.personEntityId][index] = data.coiDisclEntProjDetail;
                this.coiValidationMap.clear();
                this.relationshipChanged.emit(true);
                this.updateIsSaving(false);
                this.focusLastEditedInput();
                this.projectSFIDetails[index].isSaved = false;
            }, err => {
                setTimeout(() => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationship. Please try again.');
                    this.updateIsSaving(false);
                    this.focusLastEditedInput();
                    this.projectSFIDetails[index].isSaved = false;
                }, 1500);
            }));
        }
    }

    focusLastEditedInput() {
        setTimeout(() => {
            if (this.focusableId) {
                document?.getElementById(this.focusableId)?.focus();
                this.focusableId = '';
            }
        }, 1000);
    }

    clearValues() {
        this.coiDescription = '';
        this.coiStatusCode = null;
        this.isApplyToAllModal = false;
    }

    applyToAll() {
        this.coiValidationMap.clear();
        this.coiTableValidation.clear();
        if (!this.coiStatusCode || (this.coiStatusCode == 'null')) {
            this.coiValidationMap.set('coiStatus', 'Please select Conflict Status');
        }
        if (!this.coiDescription) {
            this.coiValidationMap.set('coiDescription', 'Please enter description');
        }
        if (this.coiValidationMap.size === 0) {
            this.saveClick();
        }
    }

    saveClick() {
        this.$subscriptions.push(this._relationShipService.saveEntityProjectRelationSFI(
            this.prepareSaveObject(), this.projectSFIDetails[0].personEntityId, this.projectSFIDetails[0].disclosureId,
            this._commonService.currentUserDetails.personId)
            .subscribe((data: any) => {
                this._relationShipService.projectSFIDetails[this.projectSFIDetails[0].personEntityId] = data.coiDisclEntProjDetails;
                hideModal('applyToAllConfirmationModal');
                this.coiDescription = '';
                this.coiStatusCode = null;
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
                this.relationshipChanged.emit(true);
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationships. Please try again.');
            }));
    }

    prepareSaveObject() {
        const REQ_ARRAY = deepCloneObject(this.projectSFIDetails);
        return REQ_ARRAY.map((ele: any) => {
            ele.projectConflictStatusCode = this.coiStatusCode;
            ele.disclComment.comment = this.coiDescription;
            ele.coiProjConflictStatusType = this.getStatusObject(ele.projectConflictStatusCode);
            return ele;
        });
    }

    updateIsSaving(newValue: boolean): void {
        this.isSaving = newValue;
        this.isSavingChange.emit(this.isSaving);
    }

    @HostListener('document:keydown.escape', ['$event'])
	handleEscapeEvent(event: any): void {
		if ((event.key === 'Escape' || event.key === 'Esc') && this.isApplyToAllModal) {
			document.getElementById('claim-sumbit-no-btn').click();
			this.clearValues();
		}
	}

    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        setTimeout(() => {
            if(this.isSlider) {
                const INFO_CARD_HEIGHT = document.getElementById('info-card')?.offsetHeight || 0;
                const HEADER_HEIGHT = document.getElementById('relationship-details')?.offsetHeight;
                const TABLE_HEADER_HEIGHT = document.getElementById('sfi-relationship-table-header')?.offsetHeight;
                document.getElementById('sfi-relationship-table-header').style.top ='-9px';
                if(window.innerWidth >= 1200) {
                    document.getElementById('sfi-relationship').style.maxHeight = (window.innerHeight - (INFO_CARD_HEIGHT + HEADER_HEIGHT + TABLE_HEADER_HEIGHT + 60)) + 'px';
                } else {
                    const HEADER_HEIGHT = document.getElementById('relationship-details-box-slider-header')?.offsetHeight;
                    document.getElementById('sfi-relationship').style.maxHeight = (window.innerHeight - (HEADER_HEIGHT +10)) + 'px';
                }
            }
        });
    }

}
