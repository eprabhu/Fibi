import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { TravelDisclosureService } from '../../services/travel-disclosure.service';

@Component({
    selector: 'app-confirmation-modal',
    templateUrl: './confirmation-modal.component.html',
    styleUrls: ['./confirmation-modal.component.scss']
})
export class ConfirmationModalComponent implements OnInit, OnDestroy {

    @Input() needDescriptionField = false;
    @Input() isMandatory = false;
    @Input() helpText = [];
    @Input() description = '';
    @Input() modalHeaderTitle = '';
    @Input() modalActionBtnName = 'Save';
    @Input() modalCloseBtnName = 'Cancel';
    @Input() descriptionError = 'Cancel';
    @Output() btnAction: EventEmitter<any> = new EventEmitter<any>();
    @Output() close: EventEmitter<any> = new EventEmitter<any>();

    mandatoryList = new Map();

    constructor(public service: TravelDisclosureService) { }

    ngOnInit() {
    }

    ngOnDestroy() {
        this.closeModal();
    }

    descriptionChangedOrEmpty() {
        if (!this.description) {
            this.mandatoryList.set('description', `${this.descriptionError}`);
        } else {
            this.mandatoryList.clear();
        }
    }

    validateDescription(): boolean {
        return this.mandatoryList.size === 0 ? true : false;
    }

    closeModal() {
        this.close.emit();
        this.mandatoryList.clear();
        this.description = '';
        document.getElementById('prop-confirmationModal-dismiss-btn').click();
    }

    performAction() {
        if (this.isMandatory) {
            this.descriptionChangedOrEmpty();
            if (this.validateDescription()) {
                this.closeModal();
                this.btnAction.emit(this.description);
            }
        } else {
            this.closeModal();
            this.btnAction.emit(this.needDescriptionField ? this.description : null);
        }
    }

}
