
function recMember(memName)
  return function (rec)
    return rec[memName]
  end
end


function newRecord()
  return {}
end


function recInsert(key)
  return function(value)
    return function(rec)
      rec[key] = value
      return rec
    end
  end
end

