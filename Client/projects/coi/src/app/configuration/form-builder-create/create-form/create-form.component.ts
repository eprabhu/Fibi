import { Component} from '@angular/core';
import { FormBuilderCreateService } from '../form-builder-create.service';
import { Router } from '@angular/router';
import { formHeader } from '../../../shared/form-builder-view/form-builder-interface';

@Component({
    selector: 'app-create-form',
    templateUrl: './create-form.component.html',
    styleUrls: ['./create-form.component.scss']
})
export class CreateFormComponent {
    title: string;
    description: string;

    constructor(private _formBuilderService: FormBuilderCreateService, private _router: Router,) { }
    
    createFormHeader() {
        this._formBuilderService.createFormHeader({ "title": this.title, "description": this.description }).subscribe((data: formHeader) => {
            this._router.navigate(['/coi/form-builder-create/form-editor'], { queryParams: { formBuilderId: data.formBuilderId } });
        })
    }

}
