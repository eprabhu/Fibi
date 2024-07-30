import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate, CanDeactivate, RouterStateSnapshot, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { FormBuilderCreateService } from '../form-builder-create/form-builder-create.service'
import { FormEditorComponent } from './form-editor/form-editor/form-editor.component';
declare const $: any;


@Injectable()
export class FormBuilderCreateRouteGaurdService implements CanActivate, CanDeactivate<FormEditorComponent> {

	constructor(private _formBuilderService: FormBuilderCreateService) { }

	canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
		return true;
	}

	canDeactivate(): boolean {
		if (!this._formBuilderService.isUnconfiguredcomponentsPresent()) {
			return true;
		} else {
			$('#unSavedChange-warning-Modal').modal('show');
		}
		return false;
	}

}
