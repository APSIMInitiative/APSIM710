#pragma hdrstop

/**************************************************************************

   File:          ClsFact.cpp

   Description:   Implements CClassFactory.

**************************************************************************/

/**************************************************************************
   #include statements
**************************************************************************/

#include "ClsFact.h"
#include "contexthandler.h"

/**************************************************************************
   private function prototypes
**************************************************************************/

/**************************************************************************
   global variables
**************************************************************************/

ULONG g_DllRefCount = 0;

///////////////////////////////////////////////////////////////////////////
//
// IClassFactory implementation
//

/**************************************************************************

   CClassFactory::CClassFactory

**************************************************************************/

CClassFactory::CClassFactory(CLSID clsid)
{
    m_clsidObject = clsid;
    m_ObjRefCount = 1;
    g_DllRefCount++;
}

/**************************************************************************

   CClassFactory::~CClassFactory

**************************************************************************/

CClassFactory::~CClassFactory()
{
    g_DllRefCount--;
}

/**************************************************************************

   CClassFactory::QueryInterface

**************************************************************************/

STDMETHODIMP CClassFactory::QueryInterface(REFIID riid, LPVOID *ppReturn)
{
    *ppReturn = NULL;

    if(IsEqualIID(riid, IID_IUnknown))
        *ppReturn = this;
    else if(IsEqualIID(riid, IID_IClassFactory))
        *ppReturn = (IClassFactory*)this;

    if(*ppReturn)
    {
        (*(LPUNKNOWN*)ppReturn)->AddRef();
        return S_OK;
    }

    return E_NOINTERFACE;
}

/**************************************************************************

   CClassFactory::AddRef

**************************************************************************/

STDMETHODIMP_(DWORD) CClassFactory::AddRef()
{
    return ++m_ObjRefCount;
}

/**************************************************************************

   CClassFactory::Release

**************************************************************************/

STDMETHODIMP_(DWORD) CClassFactory::Release()
{
    if(--m_ObjRefCount == 0)
    {
        delete this;
        return 0;
    }

    return m_ObjRefCount;
}

/**************************************************************************

   CClassFactory::CreateInstance

**************************************************************************/

STDMETHODIMP CClassFactory::CreateInstance(LPUNKNOWN pUnknown, REFIID riid,
LPVOID *ppObject)
{
    HRESULT  hResult = E_FAIL;
    LPVOID   pTemp = NULL;
    CContextMenuHandler *pContextMenu = NULL;

    *ppObject = NULL;

    if(pUnknown != NULL)
    {
        return CLASS_E_NOAGGREGATION;
    }

    // create the proper object
    if(IsEqualIID(riid, IID_IContextMenu) ||
        IsEqualIID(riid, IID_IShellExtInit))
    {
        pContextMenu = new CContextMenuHandler();
        if(pContextMenu == NULL)
        {
            return E_OUTOFMEMORY;
        }

        if(IsEqualIID(riid, IID_IContextMenu))
            pTemp = (IContextMenu*)pContextMenu;
        else if(IsEqualIID(riid, IID_IShellExtInit))
            pTemp = (IShellExtInit*)pContextMenu;
    }

    if(pTemp)
    {
        // get the QueryInterface return for our return value
        hResult = ((LPUNKNOWN)pTemp)->QueryInterface(riid, ppObject);

        // call Release to decement the ref count
//        ((LPUNKNOWN)pTemp)->Release();  // DPH - had to remove this was crashing.
    }

    return hResult;
}

/**************************************************************************

   CClassFactory::LockServer

**************************************************************************/

STDMETHODIMP CClassFactory::LockServer(BOOL)
{
    return E_NOTIMPL;
}
