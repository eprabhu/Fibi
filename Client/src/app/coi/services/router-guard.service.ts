import { Injectable } from '@angular/core';

@Injectable()
export class RouterGuardService {

    constructor() { }

    canActivate(): boolean {
        return true;
    }

}
