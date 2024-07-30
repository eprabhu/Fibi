import { Component, OnInit, OnDestroy } from '@angular/core';
import { FormBuilderCreateService } from '../form-builder-create.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { FormList} from '../form-builder-create-interface';


@Component({
    selector: 'app-form-list',
    templateUrl: './form-list.component.html',
    styleUrls: ['./form-list.component.scss']
})
export class FormListComponent implements OnInit, OnDestroy {

    formList: Array<FormList>;
    $subscriptions: Subscription[] = [];
    isReadMore = [];

    constructor(public formBuilderService: FormBuilderCreateService) { }

    ngOnInit() {
        this.loadFormList();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    loadFormList(): void {
        this.$subscriptions.push(
            this.formBuilderService.getFormList().subscribe((data: Array<FormList>) => {
                this.formList = data;
            })
        );
    }

    isFormPublished(isActive){
        this.formBuilderService.isFormPublished = isActive;
    }

}
